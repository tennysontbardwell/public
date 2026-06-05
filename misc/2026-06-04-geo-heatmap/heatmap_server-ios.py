#!/usr/bin/env python3
"""
heatmap_server.py — Interactive time-weighted Gaussian heatmap, server-recomputed
per viewport, returning the RAW LINEAR density grid (log + colormap done client-side).

Usage:
    pip install numpy            # scipy optional (faster blur; numpy fallback included)
    python heatmap_server.py data.json          # then open http://localhost:8001

Pipeline:
  startup : parse Google Timeline entries -> in-memory point masses
            (weight = dwell seconds [LINEAR in time]; per-point base_mult + probability).
  client  : on map move/zoom OR a spread-slider change, POSTs bbox + viewport size +
            spread params to /density.
  server  : pads bbox 50% per edge, filters known points to the region (+3 sigma skirt),
            splats time-mass, one separable Gaussian blur per sigma-bucket (== convolution),
            returns the LINEAR density grid as compressed float32 + metadata.
  client  : inflates the grid and applies log + colormap on a canvas; log-range and
            opacity changes are pure client-side (no server round-trip).
"""
import sys, json, math, re, struct, zlib, argparse
from datetime import datetime
from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

import numpy as np
try:
    from scipy.ndimage import gaussian_filter as _scipy_blur
except Exception:
    _scipy_blur = None

FT_TO_M    = 0.3048
SIGMA_BASE = 100 * FT_TO_M       # default 100 ft, also the fixed line-resample spacing
MAX_GRID   = 1024                # cap on returned raster dimension (px)
PAD_FRAC   = 0.5                 # server pads requested bbox by this fraction per edge
R          = 6378137.0           # Web Mercator sphere radius (m)

# ============================================================ Web Mercator (EPSG:3857)
def merc_x(lon):  return math.radians(lon) * R
def merc_y(lat):  return math.log(math.tan(math.pi / 4 + math.radians(lat) / 2)) * R
def _merc_x(lon): return np.radians(lon) * R
def _merc_y(lat): return np.log(np.tan(np.pi / 4 + np.radians(lat) / 2)) * R

# ============================================================ parsing
def _geo(s):                                 # "geo:lat,lon" -> (lon, lat)
    m = re.match(r"\s*geo:([-\d.]+),\s*([-\d.]+)", s)
    return float(m.group(2)), float(m.group(1))

def _dur_s(e):
    f = lambda t: datetime.fromisoformat(t.replace("Z", "+00:00"))
    return max((f(e["endTime"]) - f(e["startTime"])).total_seconds(), 1.0)

def _line(p0, p1, secs, base_mult, prob, out):
    # Distribute dwell linearly along a segment; FIXED ~sigma_base/2 spacing so the
    # resampling is independent of the live spread sliders (kernel applied later).
    (x0, y0), (x1, y1) = p0, p1
    mlat = math.radians((y0 + y1) / 2)
    dx = (x1 - x0) * 111320 * math.cos(mlat)
    dy = (y1 - y0) * 110540
    n = max(1, int((math.hypot(dx, dy) or 1e-6) / (SIGMA_BASE / 2)))
    for i in range(n):
        f = (i + 0.5) / n
        out.append((x0 + (x1 - x0) * f, y0 + (y1 - y0) * f, secs / n, base_mult, prob))

def parse(entries):
    """Returns list of (lon, lat, weight_seconds, base_mult, probability)."""
    pts = []
    for e in entries:
        try:
            if "visit" in e:
                v, tc = e["visit"], e["visit"]["topCandidate"]
                lon, lat = _geo(tc["placeLocation"])
                prob = float(v.get("probability", 1)) * float(tc.get("probability", 1))
                pts.append((lon, lat, _dur_s(e), 1.0, prob))
            elif "activity" in e:
                a = e["activity"]
                prob = float(a.get("topCandidate", {}).get("probability", 0.0))
                _line(_geo(a["start"]), _geo(a["end"]), _dur_s(e), 1.5, prob, pts)
            elif "timelinePath" in e:
                tp = [(_geo(p["point"]),
                       float(p["durationMinutesOffsetFromStartTime"]) * 60)
                      for p in e["timelinePath"]]
                for (g0, s0), (g1, s1) in zip(tp, tp[1:]):
                    _line(g0, g1, max(s1 - s0, 1.0), 1.0, 1.0, pts)
        except Exception as ex:
            print(f"  skip entry: {ex}", file=sys.stderr)
    return pts

# ============================================================ density (per region)
def _blur(a, sigma):
    if sigma < 0.3:
        return a
    if _scipy_blur is not None:
        return _scipy_blur(a, sigma=sigma, mode="constant")
    r = int(max(1, round(3 * sigma)))
    k = np.exp(-(np.arange(-r, r + 1) ** 2) / (2 * sigma ** 2)); k /= k.sum()
    a = np.apply_along_axis(lambda m: np.convolve(m, k, "same"), 1, a)
    return np.apply_along_axis(lambda m: np.convolve(m, k, "same"), 0, a)

class Field:
    def __init__(self, pts):
        self.LON  = np.array([p[0] for p in pts])
        self.LAT  = np.array([p[1] for p in pts])
        self.WT   = np.array([p[2] for p in pts])
        self.BM   = np.array([p[3] for p in pts])   # base multiplier (visit/activity)
        self.PROB = np.array([p[4] for p in pts])

    def bounds(self):
        return (float(self.LON.min()), float(self.LAT.min()),
                float(self.LON.max()), float(self.LAT.max()))

    def _sigmas(self, sigma_ft, conf):
        # sigma = base * base_mult * sqrt(1 + (conf*(1/p - 1))^2).
        # conf=0 ignores confidence; higher conf widens low-probability points more.
        base = sigma_ft * FT_TO_M
        p = np.clip(self.PROB, 1e-3, 0.999)
        return base * self.BM * np.sqrt(1.0 + (conf * (1.0 / p - 1.0)) ** 2)

    def density(self, w, s, e, n, vw, vh, sigma_ft, conf):
        # 1) pad requested bbox by PAD_FRAC on each edge
        dlon, dlat = (e - w) or 1e-6, (n - s) or 1e-6
        w -= PAD_FRAC * dlon; e += PAD_FRAC * dlon
        s -= PAD_FRAC * dlat; n += PAD_FRAC * dlat
        corners = [[w, n], [e, n], [e, s], [w, s]]      # TL, TR, BR, BL

        SIG = self._sigmas(sigma_ft, conf)
        lat0 = (s + n) / 2
        cphi = max(math.cos(math.radians(lat0)), 1e-6)

        # 2) filter known points to region + per-point 3-sigma skirt (no edge seam)
        mlon = 3 * SIG / (111320 * cphi); mlat = 3 * SIG / 110540
        msk = ((self.LON >= w - mlon) & (self.LON <= e + mlon) &
               (self.LAT >= s - mlat) & (self.LAT <= n + mlat))
        if not msk.any():
            return np.zeros((2, 2), np.float32), corners, 0.0

        lon, lat, wt, sig = self.LON[msk], self.LAT[msk], self.WT[msk], SIG[msk]

        # 3) build the grid in MERCATOR meters so it lines up with the basemap
        mxw, mxe = merc_x(w), merc_x(e)
        myn, mys = merc_y(n), merc_y(s)
        merc_w, merc_h = (mxe - mxw) or 1e-6, (myn - mys) or 1e-6
        ground_w, ground_h = merc_w * cphi, merc_h * cphi          # true ground meters
        Wt, Ht = min(int(vw) or 800, MAX_GRID), min(int(vh) or 800, MAX_GRID)
        cell_m = max(ground_w / Wt, ground_h / Ht, float(sig.min()) / 2.0)
        cell_merc = cell_m / cphi
        W = max(int(merc_w / cell_merc) + 1, 1)
        H = max(int(merc_h / cell_merc) + 1, 1)

        ix = np.clip(((_merc_x(lon) - mxw) / cell_merc).astype(int), 0, W - 1)
        iy = np.clip(((myn - _merc_y(lat)) / cell_merc).astype(int), 0, H - 1)
        spx = np.clip(sig / cell_m, 0.5, None)                    # sigma in px

        # 4) splat time-mass, one blur per sigma-bucket, sum (exact & LINEAR)
        keys = np.round(np.log2(spx) * 2).astype(int)
        dens = np.zeros((H, W), np.float64)
        for key in np.unique(keys):
            sel = keys == key
            g = np.zeros((H, W), np.float64)
            np.add.at(g, (iy[sel], ix[sel]), wt[sel])
            dens += _blur(g, float(np.median(spx[sel])))

        maxlog = float(math.log(float(dens.max()) + 1.0))
        return dens.astype("<f4"), corners, maxlog

# ============================================================ HTTP server
FIELD = None

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *a): pass

    def do_GET(self):
        u = urlparse(self.path)
        if u.path == "/":
            body = HTML.encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers(); self.wfile.write(body); return
        if u.path == "/density":
            q = parse_qs(u.query)
            g = lambda k, d: float(q.get(k, [d])[0])
            grid, corners, maxlog = FIELD.density(
                g("w", 0), g("s", 0), g("e", 0), g("n", 0),
                g("vw", 800), g("vh", 800), g("sigma", 100), g("conf", 1))
            body = zlib.compress(grid.tobytes(), 6)
            meta = json.dumps({"w": int(grid.shape[1]), "h": int(grid.shape[0]),
                               "corners": corners, "maxlog": maxlog})
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("X-Meta", meta)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers(); self.wfile.write(body); return
        self.send_error(404)

# ============================================================ HTML / client
HTML = r"""<!DOCTYPE html><html><head><meta charset="utf-8">
<title>Time Heatmap (raw density, server-recomputed)</title>
<meta name="viewport" content="width=device-width,initial-scale=1">
<link href="https://unpkg.com/maplibre-gl@4/dist/maplibre-gl.css" rel="stylesheet">
<script src="https://unpkg.com/maplibre-gl@4/dist/maplibre-gl.js"></script>
<script src="https://cdn.jsdelivr.net/npm/pako@2/dist/pako.min.js"></script>
<style>
 html,body,#map{margin:0;height:100%;width:100%}
 #panel{position:absolute;top:10px;left:10px;z-index:1;background:#0d1117ee;
   color:#e6edf3;padding:12px 14px;border-radius:8px;font:13px system-ui;width:240px}
 #panel label{display:block;margin:8px 0 2px} input[type=range]{width:100%}
 hr{border:0;border-top:1px solid #30363d;margin:10px 0}
 #status{font-size:11px;color:#8b949e;margin-top:6px}
</style></head><body>
<div id="map"></div>
<div id="panel">
 <b>Log density heatmap</b>
 <label><input type="checkbox" id="auto" checked> auto log range</label>
 <label>log min: <span id="lminv"></span></label><input id="lmin" type="range">
 <label>log max: <span id="lmaxv"></span></label><input id="lmax" type="range">
 <hr>
 <label>kernel &sigma; (ft): <span id="sigv">100</span></label>
 <input id="sig" type="range" min="20" max="1000" step="10" value="100">
 <label>confidence influence: <span id="confv">1.0</span></label>
 <input id="conf" type="range" min="0" max="3" step="0.1" value="1">
 <hr>
 <label>opacity: <span id="opv">0.85</span></label>
 <input id="op" type="range" min="0" max="1" step="0.05" value="0.85">
 <div id="status">init…</div>
</div>
<script>
const CFG = /*__CFG__*/null;
const $ = id => document.getElementById(id);
const lmin=$("lmin"), lmax=$("lmax"), auto=$("auto"), status=$("status");
for (const s of [lmin,lmax]){ s.min=0; s.max=CFG.globalMaxlog; s.step=CFG.globalMaxlog/200; }
lmin.value=0; lmax.value=CFG.globalMaxlog;
const labels=()=>{ $("lminv").textContent=(+lmin.value).toFixed(2);
                   $("lmaxv").textContent=(+lmax.value).toFixed(2); };
labels();

// turbo-ish colormap
const RAMP=[[48,18,59],[62,74,194],[33,144,241],[27,209,182],
            [126,238,79],[225,220,55],[253,141,39],[210,40,33]];
function color(t){ t=Math.min(Math.max(t,0),1)*(RAMP.length-1);
  const i=Math.floor(t),f=t-i,a=RAMP[i],b=RAMP[Math.min(i+1,RAMP.length-1)];
  return [a[0]+(b[0]-a[0])*f,a[1]+(b[1]-a[1])*f,a[2]+(b[2]-a[2])*f]; }

const cv=document.createElement("canvas"), ctx=cv.getContext("2d");
let cur=null;                              // {grid,w,h,corners}

function render(){                          // log + colormap, PURELY client-side
  if(!cur) return;
  if(cv.width!==cur.w||cv.height!==cur.h){ cv.width=cur.w; cv.height=cur.h; }
  const img=ctx.createImageData(cur.w,cur.h), p=img.data, g=cur.grid;
  const a=+lmin.value, span=Math.max((+lmax.value)-a,1e-6);
  for(let k=0;k<g.length;k++){
    const l=Math.log(g[k]+1.0), o=k*4;          // LOG density = the visual basis
    if(l<=a||g[k]<=0){ p[o+3]=0; continue; }
    const t=(l-a)/span, c=color(t);
    p[o]=c[0]; p[o+1]=c[1]; p[o+2]=c[2]; p[o+3]=255*Math.min(t*3,1);
  }
  ctx.putImageData(img,0,0);
  const url=cv.toDataURL(), src=map.getSource("heat");
  if(src){ src.updateImage({url, coordinates:cur.corners}); }
  else { map.addSource("heat",{type:"image",url,coordinates:cur.corners});
         map.addLayer({id:"heat",type:"raster",source:"heat",
           paint:{"raster-opacity":+$("op").value,"raster-resampling":"linear",
                  "raster-fade-duration":0}}); }
}

const map=new maplibregl.Map({container:"map",
  style:"https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
  center:CFG.center, zoom:13});
map.addControl(new maplibregl.NavigationControl());

let ctrl=null, timer=null;
function fetchRegion(){                     // needs a server round-trip
  const b=map.getBounds();
  const p=new URLSearchParams({ w:b.getWest(),s:b.getSouth(),e:b.getEast(),n:b.getNorth(),
    vw:map.getContainer().clientWidth, vh:map.getContainer().clientHeight,
    sigma:$("sig").value, conf:$("conf").value });
  if(ctrl) ctrl.abort(); ctrl=new AbortController();
  status.textContent="computing…";
  fetch("/density?"+p,{signal:ctrl.signal}).then(async r=>{
    const meta=JSON.parse(r.headers.get("X-Meta"));
    const u8=pako.inflate(new Uint8Array(await r.arrayBuffer()));
    const grid=new Float32Array(u8.buffer,u8.byteOffset,meta.w*meta.h);
    cur={grid,w:meta.w,h:meta.h,corners:meta.corners};
    if(auto.checked){ lmin.value=0; lmax.value=meta.maxlog; labels(); }
    status.textContent="peak logρ "+meta.maxlog.toFixed(2)+"  ("+meta.w+"×"+meta.h+")";
    render();
  }).catch(e=>{ if(e.name!=="AbortError") status.textContent="error"; });
}
const debFetch=()=>{ clearTimeout(timer); timer=setTimeout(fetchRegion,150); };

map.on("load", fetchRegion);
map.on("moveend", debFetch);                        // pan + zoom -> recompute region
$("sig").oninput =e=>{ $("sigv").textContent=e.target.value; debFetch(); };  // recompute
$("conf").oninput=e=>{ $("confv").textContent=(+e.target.value).toFixed(1); debFetch(); };
lmin.oninput=lmax.oninput=()=>{ auto.checked=false; labels(); render(); };    // no fetch
auto.onchange=()=>{ if(auto.checked&&cur){ render(); debFetch(); } };
$("op").oninput=e=>{ $("opv").textContent=e.target.value;
  if(map.getLayer("heat")) map.setPaintProperty("heat","raster-opacity",+e.target.value); };
</script></body></html>"""

# ============================================================ main
def load_entries(path):
    with open(path) as f:
        data = json.load(f)
    if isinstance(data, dict):
        for k in ("semanticSegments", "timelineObjects", "entries"):
            if k in data: return data[k]
        for v in data.values():
            if isinstance(v, list): return v
    return data

def main():
    global FIELD, HTML
    ap = argparse.ArgumentParser(description="Server-recomputed raw-density heatmap.")
    ap.add_argument("json", help="path to JSON file containing a list of entries")
    ap.add_argument("-p", "--port", type=int, default=8001)
    args = ap.parse_args()

    pts = parse(load_entries(args.json))
    if not pts:
        sys.exit("No usable location entries found.")
    FIELD = Field(pts)
    w, s, e, n = FIELD.bounds()
    _, _, gmax = FIELD.density(w, s, e, n, 600, 600, 100, 1)   # stable slider scale
    center = [(w + e) / 2, (s + n) / 2]
    print(f"Parsed {len(pts)} point masses;  global peak logρ = {gmax:.2f}")

    HTML = HTML.replace("/*__CFG__*/null",
                        json.dumps({"center": center, "globalMaxlog": gmax}))
    srv = ThreadingHTTPServer(("", args.port), Handler)
    print(f"Serving on http://localhost:{args.port}  (Ctrl-C to stop)")
    try: srv.serve_forever()
    except KeyboardInterrupt: pass

if __name__ == "__main__":
    main()
