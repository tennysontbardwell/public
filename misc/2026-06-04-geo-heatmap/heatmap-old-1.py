#!/usr/bin/env python3
"""
heatmap.py  —  Time-weighted Gaussian density heatmap from Google Timeline entries.

Usage:
    python heatmap.py data.json                 # writes heatmap.html
    python heatmap.py data.json -o out.html     # custom output path
    python heatmap.py data.json --serve         # serve on http://localhost:8000
    python heatmap.py data.json --serve -p 9000

Design:
  - Each entry -> weighted point mass(es): weight = dwell seconds (LINEAR in time).
  - Kernel sigma = 100 ft, widened by a confidence measure from the entry.
  - Density = sum of normalized Gaussians  ==  splat masses then ONE separable blur.
  - Density stays LINEAR until the browser; the log scale + colormap is applied
    live in JS (sliders), so the log density that drives the visual is preserved.
  - Grid cell size ~ sigma/2  => the field is band-limited (natural compression),
    then zlib+base64 embedded. MapLibre 'image' source georeferences the quad.
"""
import sys, json, math, re, zlib, base64, argparse
from datetime import datetime

import numpy as np
try:
    from scipy.ndimage import gaussian_filter as _scipy_blur
except Exception:
    _scipy_blur = None

FT_TO_M     = 0.3048
SIGMA_BASE  = 100 * FT_TO_M          # 30.48 m base stddev
MAX_GRID    = 1100                    # cap on grid dimension (px)

# ----------------------------------------------------------------------------- parse
def _geo(s):                          # "geo:lat,lon" -> (lon, lat)
    m = re.match(r"\s*geo:([-\d.]+),\s*([-\d.]+)", s)
    return float(m.group(2)), float(m.group(1))

def _dur_s(e):
    f = lambda t: datetime.fromisoformat(t.replace("Z", "+00:00"))
    return max((f(e["endTime"]) - f(e["startTime"])).total_seconds(), 1.0)

def _sigma(prob, base=SIGMA_BASE):
    # Low confidence -> extra variance -> wider kernel. sigma = hypot(base, base*(1/p-1)).
    p = max(min(float(prob), 0.999), 1e-3)
    return math.hypot(base, base * (1.0 / p - 1.0))

def _line(p0, p1, secs, sigma, out):
    # Distribute dwell linearly along a segment, sampled at ~sigma/2 spacing.
    (x0, y0), (x1, y1) = p0, p1
    mlat = math.radians((y0 + y1) / 2)
    dx = (x1 - x0) * 111320 * math.cos(mlat)
    dy = (y1 - y0) * 110540
    length = math.hypot(dx, dy) or 1e-6
    n = max(1, int(length / (sigma / 2)))
    for i in range(n):
        f = (i + 0.5) / n
        out.append((x0 + (x1 - x0) * f, y0 + (y1 - y0) * f, secs / n, sigma))

def parse(entries):
    """Returns list of (lon, lat, weight_seconds, sigma_m)."""
    pts = []
    for e in entries:
        try:
            if "visit" in e:
                v = e["visit"]; tc = v["topCandidate"]
                lon, lat = _geo(tc["placeLocation"])
                prob = float(v.get("probability", 1)) * float(tc.get("probability", 1))
                pts.append((lon, lat, _dur_s(e), _sigma(prob)))
            elif "activity" in e:
                a = e["activity"]
                prob = float(a.get("topCandidate", {}).get("probability", 0.0))
                _line(_geo(a["start"]), _geo(a["end"]), _dur_s(e),
                      _sigma(prob, base=SIGMA_BASE * 1.5), pts)
            elif "timelinePath" in e:
                t0 = datetime.fromisoformat(e["startTime"].replace("Z", "+00:00"))
                tp = [(_geo(p["point"]),
                       float(p["durationMinutesOffsetFromStartTime"]) * 60)
                      for p in e["timelinePath"]]
                for (g0, s0), (g1, s1) in zip(tp, tp[1:]):
                    _line(g0, g1, max(s1 - s0, 1.0), SIGMA_BASE, pts)
        except Exception as ex:
            print(f"  skip entry: {ex}", file=sys.stderr)
    return pts

# ----------------------------------------------------------------------------- density
def _blur(a, sigma):
    if sigma < 0.3:
        return a
    if _scipy_blur is not None:
        return _scipy_blur(a, sigma=sigma, mode="constant")
    r = int(max(1, round(3 * sigma)))
    x = np.arange(-r, r + 1)
    k = np.exp(-(x ** 2) / (2 * sigma ** 2)); k /= k.sum()
    a = np.apply_along_axis(lambda m: np.convolve(m, k, "same"), 1, a)
    a = np.apply_along_axis(lambda m: np.convolve(m, k, "same"), 0, a)
    return a

def density(pts):
    lons = np.array([p[0] for p in pts]); lats = np.array([p[1] for p in pts])
    sigs = np.array([p[3] for p in pts]); wts  = np.array([p[2] for p in pts])
    lat0 = float(lats.mean())
    mx = 111320 * math.cos(math.radians(lat0))   # m per deg lon
    my = 110540                                  # m per deg lat

    # local equirectangular meters
    X = (lons - lons.min()) * mx
    Y = (lats.max() - lats) * my                 # row 0 = north (image top)
    pad = 3 * float(sigs.max())
    w_m = X.max() + 2 * pad
    h_m = Y.max() + 2 * pad
    cell = max(w_m / MAX_GRID, h_m / MAX_GRID, float(sigs.min()) / 2.0)
    W = int(w_m / cell) + 1
    H = int(h_m / cell) + 1
    ix = ((X + pad) / cell).astype(int)
    iy = ((Y + pad) / cell).astype(int)

    # bucket by sigma (in px) so each bucket needs one blur; sum is exact & linear
    spx = np.clip(sigs / cell, 0.5, None)
    keys = np.round(np.log2(spx) * 2).astype(int)
    dens = np.zeros((H, W), np.float64)
    for key in np.unique(keys):
        sel = keys == key
        grid = np.zeros((H, W), np.float64)
        np.add.at(grid, (iy[sel], ix[sel]), wts[sel])   # splat raw time-mass
        dens += _blur(grid, float(np.median(spx[sel])))

    # corner lon/lats for the image quad (TL, TR, BR, BL)
    lon_min, lon_max = lons.min() - pad / mx, lons.min() + (W * cell - pad) / mx
    lat_max, lat_min = lats.max() + pad / my, lats.max() - (H * cell - pad) / my
    corners = [[lon_min, lat_max], [lon_max, lat_max],
               [lon_max, lat_min], [lon_min, lat_min]]
    center = [float((lon_min + lon_max) / 2), float((lat_min + lat_max) / 2)]
    return dens.astype(np.float32), corners, center

# ----------------------------------------------------------------------------- emit
def build_html(dens, corners, center):
    H, W = dens.shape
    blob = base64.b64encode(zlib.compress(dens.tobytes(), 9)).decode()
    payload = json.dumps({
        "b64": blob, "w": W, "h": H,
        "corners": corners, "center": center,
        "maxlog": float(math.log(float(dens.max()) + 1.0)),
    })
    return HTML.replace("/*__PAYLOAD__*/null", payload)

HTML = r"""<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Time Heatmap</title>
<meta name="viewport" content="width=device-width,initial-scale=1">
<link href="https://unpkg.com/maplibre-gl@4/dist/maplibre-gl.css" rel="stylesheet">
<script src="https://unpkg.com/maplibre-gl@4/dist/maplibre-gl.js"></script>
<script src="https://cdn.jsdelivr.net/npm/pako@2/dist/pako.min.js"></script>
<style>
  html,body,#map{margin:0;height:100%;width:100%}
  #panel{position:absolute;top:10px;left:10px;z-index:1;background:#0d1117ee;
    color:#e6edf3;padding:12px 14px;border-radius:8px;font:13px system-ui;width:230px}
  #panel label{display:block;margin:8px 0 2px}
  input[type=range]{width:100%}
</style></head>
<body>
<div id="map"></div>
<div id="panel">
  <b>Log density heatmap</b>
  <label>log min: <span id="lminv"></span></label><input id="lmin" type="range">
  <label>log max: <span id="lmaxv"></span></label><input id="lmax" type="range">
  <label>opacity: <span id="opv">0.85</span></label>
  <input id="op" type="range" min="0" max="1" step="0.05" value="0.85">
</div>
<script>
const D = /*__PAYLOAD__*/null;

// --- decode embedded linear density grid ---
const raw = pako.inflate(Uint8Array.from(atob(D.b64), c => c.charCodeAt(0)));
const dens = new Float32Array(raw.buffer, raw.byteOffset, D.w * D.h);

// --- turbo-ish colormap ---
const RAMP = [[48,18,59],[62,74,194],[33,144,241],[27,209,182],
              [126,238,79],[225,220,55],[253,141,39],[210,40,33]];
function color(t){
  t = Math.min(Math.max(t,0),1) * (RAMP.length-1);
  const i = Math.floor(t), f = t - i, a = RAMP[i], b = RAMP[Math.min(i+1,RAMP.length-1)];
  return [a[0]+(b[0]-a[0])*f, a[1]+(b[1]-a[1])*f, a[2]+(b[2]-a[2])*f];
}

const cv = document.createElement("canvas");
cv.width = D.w; cv.height = D.h;
const ctx = cv.getContext("2d");
const img = ctx.createImageData(D.w, D.h);

const map = new maplibregl.Map({
  container: "map",
  style: "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
  center: D.center, zoom: 13,
});
map.addControl(new maplibregl.NavigationControl());
map.on("load", () => {
  map.addSource("heat", { type: "image", url: cv.toDataURL(), coordinates: D.corners });
  map.addLayer({ id: "heat", type: "raster", source: "heat",
    paint: { "raster-opacity": 0.85, "raster-resampling": "linear",
             "raster-fade-duration": 0 } });
  map.fitBounds([D.corners[3], D.corners[1]], { padding: 40, duration: 0 });
});


function render(){
  const lmin = +document.getElementById("lmin").value;
  const lmax = +document.getElementById("lmax").value;
  document.getElementById("lminv").textContent = lmin.toFixed(2);
  document.getElementById("lmaxv").textContent = lmax.toFixed(2);
  const span = Math.max(lmax - lmin, 1e-6), p = img.data;
  for (let k = 0; k < dens.length; k++){
    const l = Math.log(dens[k] + 1.0);          // LOG density (the visual basis)
    const t = (l - lmin) / span, o = k * 4;
    if (l <= lmin || dens[k] <= 0){ p[o+3] = 0; continue; }
    const c = color(t);
    p[o]=c[0]; p[o+1]=c[1]; p[o+2]=c[2];
    p[o+3] = 255 * Math.min(t * 3, 1);          // fade in near zero
  }
  ctx.putImageData(img, 0, 0);
  const src = map.getSource("heat");
  if (src) src.updateImage({ url: cv.toDataURL() });
}

const lmin = document.getElementById("lmin"), lmax = document.getElementById("lmax");
for (const s of [lmin, lmax]){ s.min = 0; s.max = D.maxlog; s.step = D.maxlog/200; }
lmin.value = 0; lmax.value = D.maxlog;
render();

lmin.oninput = lmax.oninput = render;
document.getElementById("op").oninput = e => {
  document.getElementById("opv").textContent = e.target.value;
  if (map.getLayer("heat")) map.setPaintProperty("heat","raster-opacity", +e.target.value);
};
</script></body></html>"""

# ----------------------------------------------------------------------------- main
def load_entries(path):
    with open(path) as f:
        data = json.load(f)
    if isinstance(data, dict):                       # Google export wrappers
        for k in ("semanticSegments", "timelineObjects", "entries"):
            if k in data: return data[k]
        for v in data.values():
            if isinstance(v, list): return v
    return data

def main():
    ap = argparse.ArgumentParser(description="Time-weighted Gaussian heatmap.")
    ap.add_argument("json", help="path to JSON file containing a list of entries")
    ap.add_argument("-o", "--out", default="heatmap.html")
    ap.add_argument("--serve", action="store_true", help="serve instead of writing")
    ap.add_argument("-p", "--port", type=int, default=8000)
    args = ap.parse_args()

    pts = parse(load_entries(args.json))
    if not pts:
        sys.exit("No usable location entries found.")
    print(f"Parsed {len(pts)} point masses.")
    dens, corners, center = density(pts)
    print(f"Density grid: {dens.shape[1]}x{dens.shape[0]}, peak={dens.max():.1f}s")
    html = build_html(dens, corners, center)

    if args.serve:
        import http.server, socketserver
        class H(http.server.BaseHTTPRequestHandler):
            def do_GET(self):
                self.send_response(200)
                self.send_header("Content-Type", "text/html"); self.end_headers()
                self.wfile.write(html.encode())
            def log_message(self, *a): pass
        with socketserver.TCPServer(("", args.port), H) as srv:
            print(f"Serving on http://localhost:{args.port}  (Ctrl-C to stop)")
            try: srv.serve_forever()
            except KeyboardInterrupt: pass
    else:
        with open(args.out, "w") as f:
            f.write(html)
        print(f"Wrote {args.out} — open it in a browser.")

if __name__ == "__main__":
    main()
