#!/usr/bin/env python3
"""
heatmap_server.py — Interactive, server-recomputed time-weighted Gaussian heatmap.

Usage:
    pip install numpy            # scipy optional (faster blur; numpy fallback included)
    python heatmap_server.py data.json
    python heatmap_server.py data.json -p 8000
    # then open http://localhost:8000

Architecture:
  - Startup: parse all Google Timeline entries -> in-memory point masses
    (weight = dwell seconds [LINEAR in time], sigma = 100 ft widened by confidence).
  - Client: on every map move/zoom (moveend), POSTs the current bounding box +
    viewport size + slider range to /density.
  - Server: pads the bbox by 50% on each edge, filters the points it knows about
    (plus a 3-sigma skirt so blur from just-offscreen sources is correct), splats
    the time-mass, runs one separable Gaussian blur per sigma-bucket (== convolution),
    colorizes with a LOG colormap, and returns a PNG + the region's log range.
  - Client replaces the image overlay and, if in auto mode, updates the sliders.
"""
import sys, json, math, re, struct, zlib, base64, argparse
from datetime import datetime
from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

import numpy as np
try:
    from scipy.ndimage import gaussian_filter as _scipy_blur
except Exception:
    _scipy_blur = None

FT_TO_M    = 0.3048
SIGMA_BASE = 100 * FT_TO_M      # 30.48 m base stddev
MAX_GRID   = 1024               # cap on returned raster dimension (px)
PAD_FRAC   = 0.5                # server pads requested bbox by this fraction per edge

# ============================================================ parsing
def _geo(s):                                # "geo:lat,lon" -> (lon, lat)
    m = re.match(r"\s*geo:([-\d.]+),\s*([-\d.]+)", s)
    return float(m.group(2)), float(m.group(1))

def _dur_s(e):
    f = lambda t: datetime.fromisoformat(t.replace("Z", "+00:00"))
    return max((f(e["endTime"]) - f(e["startTime"])).total_seconds(), 1.0)

def _sigma(prob, base=SIGMA_BASE):
    p = max(min(float(prob), 0.999), 1e-3)
    return math.hypot(base, base * (1.0 / p - 1.0))   # low conf -> wider kernel

def _line(p0, p1, secs, sigma, out):
    (x0, y0), (x1, y1) = p0, p1
    mlat = math.radians((y0 + y1) / 2)
    dx = (x1 - x0) * 111320 * math.cos(mlat)
    dy = (y1 - y0) * 110540
    n = max(1, int((math.hypot(dx, dy) or 1e-6) / (sigma / 2)))
    for i in range(n):
        f = (i + 0.5) / n
        out.append((x0 + (x1 - x0) * f, y0 + (y1 - y0) * f, secs / n, sigma))

def parse(entries):
    pts = []
    for e in entries:
        try:
            if "visit" in e:
                v, tc = e["visit"], e["visit"]["topCandidate"]
                lon, lat = _geo(tc["placeLocation"])
                prob = float(v.get("probability", 1)) * float(tc.get("probability", 1))
                pts.append((lon, lat, _dur_s(e), _sigma(prob)))
            elif "activity" in e:
                a = e["activity"]
                prob = float(a.get("topCandidate", {}).get("probability", 0.0))
                _line(_geo(a["start"]), _geo(a["end"]), _dur_s(e),
                      _sigma(prob, base=SIGMA_BASE * 1.5), pts)
            elif "timelinePath" in e:
                tp = [(_geo(p["point"]),
                       float(p["durationMinutesOffsetFromStartTime"]) * 60)
                      for p in e["timelinePath"]]
                for (g0, s0), (g1, s1) in zip(tp, tp[1:]):
                    _line(g0, g1, max(s1 - s0, 1.0), SIGMA_BASE, pts)
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
    """Holds all point masses as numpy arrays; computes density for any bbox."""
    def __init__(self, pts):
        self.LON = np.array([p[0] for p in pts])
        self.LAT = np.array([p[1] for p in pts])
        self.WT  = np.array([p[2] for p in pts])
        self.SIG = np.array([p[3] for p in pts])

    def bounds(self):
        return (float(self.LON.min()), float(self.LAT.min()),
                float(self.LON.max()), float(self.LAT.max()))

    def density(self, w, s, e, n, vw, vh):
        # 1) pad the requested bbox by PAD_FRAC on each edge
        dlon, dlat = (e - w) or 1e-6, (n - s) or 1e-6
        w -= PAD_FRAC * dlon; e += PAD_FRAC * dlon
        s -= PAD_FRAC * dlat; n += PAD_FRAC * dlat

        lat0 = (s + n) / 2
        mx = 111320 * math.cos(math.radians(lat0)); my = 110540

        # 2) filter known points to this region + a 3-sigma skirt (per point)
        mlon = 3 * self.SIG / mx; mlat = 3 * self.SIG / my
        m = ((self.LON >= w - mlon) & (self.LON <= e + mlon) &
             (self.LAT >= s - mlat) & (self.LAT <= n + mlat))
        corners = [[w, n], [e, n], [e, s], [w, s]]    # TL, TR, BR, BL
        if not m.any():
            return np.zeros((2, 2), np.float32), corners, 0.0

        lon, lat = self.LON[m], self.LAT[m]
        wt,  sig = self.WT[m],  self.SIG[m]

        # 3) pick raster resolution: match viewport, capped, never finer than sigma/2
        w_m, h_m = (e - w) * mx, (n - s) * my
        Wt = min(int(vw) or 800, MAX_GRID); Ht = min(int(vh) or 800, MAX_GRID)
        cell = max(w_m / Wt, h_m / Ht, float(sig.min()) / 2.0)
        W = max(int(w_m / cell) + 1, 1); H = max(int(h_m / cell) + 1, 1)

        ix = np.clip(((lon - w) * mx / cell).astype(int), 0, W - 1)
        iy = np.clip(((n - lat) * my / cell).astype(int), 0, H - 1)  # row0 = north
        spx = np.clip(sig / cell, 0.5, None)

        # 4) splat time-mass, one blur per sigma-bucket, sum (exact & linear)
        keys = np.round(np.log2(spx) * 2).astype(int)
        dens = np.zeros((H, W), np.float64)
        for key in np.unique(keys):
            sel = keys == key
            g = np.zeros((H, W), np.float64)
            np.add.at(g, (iy[sel], ix[sel]), wt[sel])
            dens += _blur(g, float(np.median(spx[sel])))

        maxlog = float(math.log(float(dens.max()) + 1.0))
        return dens.astype(np.float32), corners, maxlog

# ============================================================ colorize + PNG
RAMP = np.array([[48,18,59],[62,74,194],[33,144,241],[27,209,182],
                 [126,238,79],[225,220,55],[253,141,39],[210,40,33]], float)

def colorize(dens, lmin, lmax):
    l = np.log(dens + 1.0)                       # LOG density (the visual basis)
    t = np.clip((l - lmin) / max(lmax - lmin, 1e-6), 0, 1)
    pos = np.linspace(0, 1, len(RAMP))
    rgb = np.stack([np.interp(t, pos, RAMP[:, c]) for c in range(3)], -1)
    a = np.clip(t * 3, 0, 1) * 255               # fade in near the floor
    a[(l <= lmin) | (dens <= 0)] = 0
    return np.dstack([rgb, a]).astype(np.uint8)

def encode_png(rgba):
    h, w, _ = rgba.shape
    def chunk(typ, data):
        return (struct.pack(">I", len(data)) + typ + data +
                struct.pack(">I", zlib.crc32(typ + data) & 0xffffffff))
    raw = bytearray()
    flat = rgba.tobytes(); stride = w * 4
    for y in range(h):
        raw.append(0)                            # filter type 0
        raw += flat[y * stride:(y + 1) * stride]
    return (b"\x89PNG\r\n\x1a\n" +
            chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 6, 0, 0, 0)) +
            chunk(b"IDAT", zlib.compress(bytes(raw), 6)) +
            chunk(b"IEND", b""))

# ============================================================ HTTP server
FIELD = None
GLOBAL_MAXLOG = 1.0
GLOBAL_BOUNDS = (0, 0, 0, 0)

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *a): pass

    def _json(self, obj):
        body = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers(); self.wfile.write(body)

    def do_GET(self):
        u = urlparse(self.path)
        if u.path == "/":
            body = HTML.encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers(); self.wfile.write(body); return
        if u.path == "/density":
            print("request!")
            q = parse_qs(u.query)
            g = lambda k, d=0.0: float(q.get(k, [d])[0])
            dens, corners, maxlog = FIELD.density(
                g("w"), g("s"), g("e"), g("n"), g("vw", 800), g("vh", 800))
            auto = q.get("lmax", ["auto"])[0] == "auto"
            lmin = 0.0 if auto else g("lmin")
            lmax = maxlog if auto else g("lmax")
            png = encode_png(colorize(dens, lmin, lmax))
            self._json({
                "png": "data:image/png;base64," + base64.b64encode(png).decode(),
                "corners": corners, "maxlog": maxlog,
                "lmin": lmin, "lmax": lmax,
            });
            print("done!")
            return
        self.send_error(404)

# ============================================================ HTML / client
HTML = r"""<!DOCTYPE html><html><head><meta charset="utf-8">
<title>Time Heatmap (server-recomputed)</title>
<meta name="viewport" content="width=device-width,initial-scale=1">
<link href="https://unpkg.com/maplibre-gl@4/dist/maplibre-gl.css" rel="stylesheet">
<script src="https://unpkg.com/maplibre-gl@4/dist/maplibre-gl.js"></script>
<style>
 html,body,#map{margin:0;height:100%;width:100%}
 #panel{position:absolute;top:10px;left:10px;z-index:1;background:#0d1117ee;
   color:#e6edf3;padding:12px 14px;border-radius:8px;font:13px system-ui;width:230px}
 #panel label{display:block;margin:8px 0 2px} input[type=range]{width:100%}
 #status{font-size:11px;color:#8b949e;margin-top:6px}
</style></head><body>
<div id="map"></div>
<div id="panel">
 <b>Log density heatmap</b>
 <label><input type="checkbox" id="auto" checked> auto range</label>
 <label>log min: <span id="lminv"></span></label><input id="lmin" type="range">
 <label>log max: <span id="lmaxv"></span></label><input id="lmax" type="range">
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
function labels(){ $("lminv").textContent=(+lmin.value).toFixed(2);
                   $("lmaxv").textContent=(+lmax.value).toFixed(2); }
labels();

const map = new maplibregl.Map({
  container:"map",
  style:"https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
  center:CFG.center, zoom:13 });
map.addControl(new maplibregl.NavigationControl());

let ctrl=null, timer=null;
function fetchRegion(){
  const b=map.getBounds();
  const p=new URLSearchParams({
    w:b.getWest(), s:b.getSouth(), e:b.getEast(), n:b.getNorth(),
    vw:map.getContainer().clientWidth, vh:map.getContainer().clientHeight });
  if (auto.checked){ p.set("lmax","auto"); }
  else { p.set("lmin",lmin.value); p.set("lmax",lmax.value); }
  if (ctrl) ctrl.abort();
  ctrl=new AbortController();
  status.textContent="computing…";
  fetch("/density?"+p, {signal:ctrl.signal})
   .then(r=>r.json())
   .then(d=>{
     const src=map.getSource("heat");
     if (src){ src.updateImage({url:d.png, coordinates:d.corners}); }
     else {
       map.addSource("heat",{type:"image",url:d.png,coordinates:d.corners});
       map.addLayer({id:"heat",type:"raster",source:"heat",
         paint:{"raster-opacity":+$("op").value,"raster-resampling":"linear",
                "raster-fade-duration":0}});
     }
     if (auto.checked){ lmin.value=d.lmin; lmax.value=d.lmax; labels(); }
     status.textContent="peak logρ "+d.maxlog.toFixed(2);
   })
   .catch(e=>{ if(e.name!=="AbortError") status.textContent="error"; });
}
const debounced=()=>{ clearTimeout(timer); timer=setTimeout(fetchRegion,150); };

map.on("load", fetchRegion);
map.on("moveend", debounced);          // covers pan + zoom
lmin.oninput=lmax.oninput=()=>{ auto.checked=false; labels(); debounced(); };
auto.onchange=()=>{ if(auto.checked) debounced(); };
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
    global FIELD, GLOBAL_MAXLOG, GLOBAL_BOUNDS, HTML
    ap = argparse.ArgumentParser(description="Server-recomputed time-weighted heatmap.")
    ap.add_argument("json", help="path to JSON file containing a list of entries")
    ap.add_argument("-p", "--port", type=int, default=8000)
    args = ap.parse_args()

    pts = parse(load_entries(args.json))
    if not pts:
        sys.exit("No usable location entries found.")
    FIELD = Field(pts)
    GLOBAL_BOUNDS = FIELD.bounds()
    w, s, e, n = GLOBAL_BOUNDS
    _, _, GLOBAL_MAXLOG = FIELD.density(w, s, e, n, 600, 600)  # stable slider scale
    center = [(w + e) / 2, (s + n) / 2]
    print(f"Parsed {len(pts)} point masses;  global peak logρ = {GLOBAL_MAXLOG:.2f}")

    HTML = HTML.replace("/*__CFG__*/null", json.dumps({
        "center": center, "globalMaxlog": GLOBAL_MAXLOG}))

    srv = ThreadingHTTPServer(("", args.port), Handler)
    print(f"Serving on http://localhost:{args.port}  (Ctrl-C to stop)")
    try: srv.serve_forever()
    except KeyboardInterrupt: pass

if __name__ == "__main__":
    main()
