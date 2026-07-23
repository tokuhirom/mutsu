#!/usr/bin/env python3
"""Render bench-history.tsv into a self-contained HTML trend dashboard.

The bench CI appends one row per (commit, benchmark) to `bench-history.tsv` on
the `bench-data` branch, but the raw TSV makes the *trend* hard to read. This
turns it into small-multiple line charts — one mini chart per benchmark, with
the interpreter (MUTSU_JIT=off) and +jit series over commit order — so a
regression or win is visible at a glance.

Usage:
    # open directly in a browser (standards mode via --standalone):
    git show origin/bench-data:bench-history.tsv | \
        python3 scripts/bench-visualize.py --standalone -o bench-trend.html
    # or from a local copy of the TSV:
    python3 scripts/bench-visualize.py bench-history.tsv --standalone -o bench-trend.html

The output is a single HTML file with no external requests (charts are drawn as
inline SVG by a small vanilla-JS runtime), so it opens offline. Omit
`--standalone` when the HTML will be wrapped by something that supplies its own
document skeleton (e.g. published as an Artifact).

Pass `--site-chrome` when the output is deployed into `wasm-demo/`: it adds the
site's shared nav and footer by loading `assets/site.css` and `assets/i18n.js`
from the same directory, so the dashboard is a page of the site rather than an
orphan. That trades away self-containedness, which is why it is opt-in.
"""

import argparse
import html
import json
import sys

COLUMNS = [
    "date",
    "commit",
    "benchmark",
    "mutsu_median_s",
    "mutsu_min_s",
    "raku_median_s",
    "ratio_mutsu_over_raku",
    "runs",
    "runner",
    "rakudo",
]


def parse_rows(text):
    rows = []
    for i, line in enumerate(text.splitlines()):
        if not line.strip():
            continue
        parts = line.split("\t")
        if i == 0 and parts[0] == "date":
            continue  # header
        if len(parts) < 7:
            continue
        rec = dict(zip(COLUMNS, parts))
        try:
            rec["mutsu_median_s"] = float(rec["mutsu_median_s"])
            rec["ratio"] = float(rec["ratio_mutsu_over_raku"])
        except (ValueError, KeyError):
            continue
        rows.append(rec)
    return rows


def build_model(rows):
    # Chronological commit order: each distinct commit keyed by first-seen date.
    first_seen = {}
    for r in rows:
        c = r["commit"]
        if c not in first_seen or r["date"] < first_seen[c]:
            first_seen[c] = r["date"]
    order = sorted(first_seen, key=lambda c: (first_seen[c], c))
    idx = {c: i for i, c in enumerate(order)}
    commit_meta = [{"sha": c[:9], "date": first_seen[c]} for c in order]

    # benchmark basename -> {"base": [...pts], "jit": [...pts]}
    benches = {}
    for r in rows:
        name = r["benchmark"]
        lane = "jit" if name.endswith("+jit") else "base"
        base_name = name[:-4] if lane == "jit" else name
        b = benches.setdefault(base_name, {"base": {}, "jit": {}})
        # last write wins if a commit reran a benchmark
        b[lane][idx[r["commit"]]] = [
            idx[r["commit"]],
            round(r["mutsu_median_s"], 5),
            round(r["ratio"], 4),
        ]

    out = []
    for name in sorted(benches):
        base = [benches[name]["base"][k] for k in sorted(benches[name]["base"])]
        jit = [benches[name]["jit"][k] for k in sorted(benches[name]["jit"])]
        out.append({"name": name, "base": base, "jit": jit})
    return {"commits": commit_meta, "benches": out}


TEMPLATE = r"""<title>Benchmark trend &mdash; mutsu</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
__CHROME_HEAD__
<style>
/* Palette shared with the rest of the site (wasm-demo/assets/site.css), so the
   dashboard reads as the same product whether it is deployed alongside the
   site or opened as a standalone file. */
:root {
  --surface: #1a0a20; --card: #2a1235; --ink: #f0e6f6; --muted: #9a7fa8;
  --hair: #3a1c46; --grid: #33163f; --edge: #4a2555;
  --base: #9fd6ff; --jit: #ffc48a; --good: #7deba0; --bad: #ff6b8a;
  --accent: #E91E8C;
  --shadow: 0 1px 2px rgba(0,0,0,.3), 0 1px 3px rgba(0,0,0,.35);
}
* { box-sizing: border-box; }
body {
  margin: 0; color: var(--ink);
  background: linear-gradient(135deg, var(--surface) 0%, #2d1040 50%, #1a0a30 100%);
  background-attachment: fixed;
  font-family: system-ui, -apple-system, 'Hiragino Sans', 'Noto Sans JP', sans-serif;
  font-size: 14px; line-height: 1.5; -webkit-font-smoothing: antialiased;
  min-height: 100vh; display: flex; flex-direction: column;
}
.mono { font-family: 'Fira Code', 'JetBrains Mono', ui-monospace, Menlo, monospace;
  font-variant-numeric: tabular-nums; }
.bench-head {
  position: sticky; top: 0; z-index: 5; background: rgba(13, 5, 20, .75);
  backdrop-filter: blur(8px); border-bottom: 1px solid var(--edge);
  padding: 18px clamp(16px, 4vw, 40px) 14px;
}
/* Deployed alongside the site, the site's own nav already owns the top of the
   viewport, so the controls scroll away with the content instead of fighting
   it for the sticky slot. */
.site-nav ~ .bench-head { position: static; }
.title { display: flex; align-items: baseline; gap: 12px; flex-wrap: wrap; }
h1 { font-size: 19px; font-weight: 650; margin: 0; letter-spacing: -.01em; }
.bench-head .sub { color: var(--muted); font-size: 12.5px; }
.controls { display: flex; gap: 18px; flex-wrap: wrap; align-items: center; margin-top: 14px; }
.seg { display: inline-flex; border: 1px solid var(--edge); border-radius: 8px; overflow: hidden; }
.seg button {
  appearance: none; border: 0; background: transparent; color: var(--muted);
  font: inherit; font-size: 12.5px; padding: 6px 12px; cursor: pointer;
}
.seg button[aria-pressed="true"] { background: var(--accent); color: #fff; }
.seg button:hover { color: var(--ink); }
.seg button:focus-visible { outline: 2px solid var(--base); outline-offset: -2px; }
.ctl-label { font-size: 11px; text-transform: uppercase; letter-spacing: .06em; color: var(--muted); margin-right: 2px; }
.legend { display: flex; gap: 16px; align-items: center; margin-left: auto; font-size: 12.5px; }
.legend span { display: inline-flex; align-items: center; gap: 6px; color: var(--muted); }
.swatch { width: 14px; height: 3px; border-radius: 2px; display: inline-block; }
main { padding: clamp(16px, 3vw, 28px) clamp(16px, 4vw, 40px) 60px; flex: 1; }
.grid { display: grid; gap: 14px; grid-template-columns: repeat(auto-fill, minmax(310px, 1fr)); }
/* Namespaced: site.css also defines `.card`, and this file is loaded next to
   it on the deployed site. */
.bench-card {
  background: var(--card); border: 1px solid var(--edge); border-radius: 12px;
  padding: 13px 14px 8px; box-shadow: var(--shadow); position: relative;
}
.bench-card h2 { font-size: 13.5px; font-weight: 600; margin: 0; letter-spacing: -.005em; }
.bench-card .head { display: flex; justify-content: space-between; align-items: baseline; gap: 8px; }
.readout { display: flex; align-items: baseline; gap: 8px; margin: 2px 0 4px; }
.now { font-size: 16px; font-weight: 600; }
.now .unit { font-size: 11px; color: var(--muted); font-weight: 400; margin-left: 1px; }
.chip { font-size: 11px; font-weight: 600; padding: 1px 6px; border-radius: 6px;
  border: 1px solid transparent; }
.chip.up   { color: var(--bad);  background: color-mix(in srgb, var(--bad) 12%, transparent); }
.chip.down { color: var(--good); background: color-mix(in srgb, var(--good) 12%, transparent); }
.chip.flat { color: var(--muted); background: color-mix(in srgb, var(--muted) 12%, transparent); }
svg { display: block; width: 100%; height: auto; overflow: visible; touch-action: none; }
.gridline { stroke: var(--grid); stroke-width: 1; }
.refline { stroke: var(--muted); stroke-width: 1; stroke-dasharray: 3 3; opacity: .5; }
.serie { fill: none; stroke-width: 2; stroke-linejoin: round; stroke-linecap: round; }
.serie.base { stroke: var(--base); } .serie.jit { stroke: var(--jit); }
.dot-end { stroke: var(--card); stroke-width: 1.5; }
.dot-end.base { fill: var(--base); } .dot-end.jit { fill: var(--jit); }
.crosshair { stroke: var(--muted); stroke-width: 1; opacity: .55; }
.tip {
  position: absolute; pointer-events: none; z-index: 3; background: var(--ink);
  color: var(--surface); font-size: 11px; padding: 5px 8px; border-radius: 6px;
  white-space: nowrap; opacity: 0; transform: translate(-50%, -100%); transition: opacity .08s;
  box-shadow: 0 2px 8px rgba(0,0,0,.25);
}
.tip .r { color: color-mix(in srgb, var(--surface) 62%, transparent); }
.axis { font-size: 9.5px; fill: var(--muted); }
table { border-collapse: collapse; width: 100%; font-size: 12.5px; }
th, td { text-align: right; padding: 6px 10px; border-bottom: 1px solid var(--hair); }
th:first-child, td:first-child { text-align: left; }
thead th { position: sticky; top: 0; background: var(--card); color: var(--muted);
  font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: .05em; cursor: pointer; }
tbody tr:hover { background: color-mix(in srgb, var(--ink) 8%, transparent); }
.hidden { display: none; }
.foot { color: var(--muted); font-size: 11.5px; margin-top: 22px; }
</style>
__CHROME_NAV__
<div class="bench-head">
  <div class="title">
    <h1>Benchmark trend</h1>
    <span class="sub" id="meta"></span>
  </div>
  <div class="controls">
    <span class="ctl-label">Metric</span>
    <div class="seg" id="metric" role="group" aria-label="Metric">
      <button data-v="seconds" aria-pressed="true">mutsu seconds</button>
      <button data-v="ratio" aria-pressed="false">ratio vs raku</button>
    </div>
    <span class="ctl-label">Window</span>
    <div class="seg" id="window" role="group" aria-label="Commit window">
      <button data-v="50" aria-pressed="false">50</button>
      <button data-v="150" aria-pressed="false">150</button>
      <button data-v="0" aria-pressed="true">all</button>
    </div>
    <span class="ctl-label">View</span>
    <div class="seg" id="view" role="group" aria-label="View">
      <button data-v="charts" aria-pressed="true">charts</button>
      <button data-v="table" aria-pressed="false">table</button>
    </div>
    <div class="legend">
      <span><i class="swatch" style="background:var(--base)"></i>interpreter</span>
      <span><i class="swatch" style="background:var(--jit)"></i>+jit</span>
    </div>
  </div>
</div>
<main>
  <div class="grid" id="grid"></div>
  <div id="tableWrap" class="hidden"></div>
  <p class="foot" id="foot"></p>
</main>
__CHROME_FOOTER__

<script id="data" type="application/json">__DATA__</script>
<script>
const DATA = JSON.parse(document.getElementById('data').textContent);
const commits = DATA.commits, benches = DATA.benches;
const N = commits.length;
let metric = 'seconds', windowN = 0, view = 'charts';

const yval = (pt) => metric === 'seconds' ? pt[1] : pt[2];
const fmt = (v) => metric === 'seconds'
  ? (v < 0.001 ? v.toExponential(1) : v.toFixed(v < 0.1 ? 4 : 3))
  : v.toFixed(2);
const unit = () => metric === 'seconds' ? 's' : '×';

function windowStart() { return windowN > 0 ? Math.max(0, N - windowN) : 0; }

// Percent change of the last point vs the previous, in the active window.
function delta(pts) {
  const w = pts.filter(p => p[0] >= windowStart());
  if (w.length < 2) return null;
  const a = yval(w[w.length - 2]), b = yval(w[w.length - 1]);
  if (!a) return null;
  return { pct: (b - a) / a * 100, now: b };
}

const W = 300, H = 96, PADL = 6, PADR = 10, PADT = 10, PADB = 16;

function chart(b) {
  const x0 = windowStart(), span = Math.max(1, (N - 1) - x0);
  const sx = (i) => PADL + (i - x0) / span * (W - PADL - PADR);
  const lanes = [['base', b.base], ['jit', b.jit]]
    .map(([k, pts]) => [k, pts.filter(p => p[0] >= x0)]);
  let lo = Infinity, hi = -Infinity;
  for (const [, pts] of lanes) for (const p of pts) {
    const v = yval(p); if (v < lo) lo = v; if (v > hi) hi = v;
  }
  if (metric === 'ratio') { lo = Math.min(lo, 1); hi = Math.max(hi, 1); }
  if (!isFinite(lo)) { lo = 0; hi = 1; }
  if (hi === lo) { hi = lo + 1; }
  const pad = (hi - lo) * 0.12; lo -= pad; hi += pad;
  const sy = (v) => PADT + (1 - (v - lo) / (hi - lo)) * (H - PADT - PADB);

  let svg = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="${b.name} trend">`;
  for (const t of [0.5]) { const y = PADT + t * (H - PADT - PADB);
    svg += `<line class="gridline" x1="${PADL}" x2="${W - PADR}" y1="${y}" y2="${y}"/>`; }
  if (metric === 'ratio' && 1 >= lo && 1 <= hi) {
    const y = sy(1);
    svg += `<line class="refline" x1="${PADL}" x2="${W - PADR}" y1="${y}" y2="${y}"/>`;
    svg += `<text class="axis" x="${W - PADR}" y="${y - 3}" text-anchor="end">raku</text>`;
  }
  svg += `<text class="axis" x="${PADL}" y="${PADT - 2}">${fmt(hi)}${unit()}</text>`;
  svg += `<text class="axis" x="${PADL}" y="${H - 4}">${fmt(lo)}${unit()}</text>`;
  for (const [k, pts] of lanes) {
    if (!pts.length) continue;
    const d = pts.map((p, i) => `${i ? 'L' : 'M'}${sx(p[0]).toFixed(1)} ${sy(yval(p)).toFixed(1)}`).join(' ');
    svg += `<path class="serie ${k}" d="${d}"/>`;
    const last = pts[pts.length - 1];
    svg += `<circle class="dot-end ${k}" cx="${sx(last[0]).toFixed(1)}" cy="${sy(yval(last)).toFixed(1)}" r="2.6"/>`;
  }
  svg += `<line class="crosshair" x1="0" x2="0" y1="${PADT}" y2="${H - PADB}" style="opacity:0"/>`;
  svg += `</svg>`;
  return { svg, sx, sy, x0, span };
}

function cardHTML(b) {
  const c = chart(b);
  const dj = delta(b.jit), db = delta(b.base);
  const d = dj || db;
  let chip = '<span class="chip flat">—</span>', now = '', nowLane = dj ? 'jit' : 'base';
  if (d) {
    const cls = Math.abs(d.pct) < 0.5 ? 'flat' : (d.pct > 0 ? 'up' : 'down');
    const sign = d.pct > 0 ? '+' : '';
    const arrow = metric === 'seconds' ? (d.pct > 0 ? ' slower' : (d.pct < 0 ? ' faster' : '')) : '';
    chip = `<span class="chip ${cls}">${sign}${d.pct.toFixed(1)}%${cls==='flat'?'':arrow}</span>`;
    now = `<span class="now mono">${fmt(d.now)}<span class="unit">${unit()}${nowLane==='jit'?' (+jit)':''}</span></span>`;
  }
  return `<div class="bench-card" data-name="${b.name}">
    <div class="head"><h2>${b.name}</h2></div>
    <div class="readout">${now}${chip}</div>
    ${c.svg}
    <div class="tip"></div>
  </div>`;
}

function renderCharts() {
  const grid = document.getElementById('grid');
  grid.innerHTML = benches.map(cardHTML).join('');
  benches.forEach((b, i) => wireHover(grid.children[i], b));
}

function wireHover(card, b) {
  const svg = card.querySelector('svg'), tip = card.querySelector('.tip');
  const cross = card.querySelector('.crosshair');
  const x0 = windowStart(), span = Math.max(1, (N - 1) - x0);
  const all = [...b.base.map(p => ['base', p]), ...b.jit.map(p => ['jit', p])].filter(([, p]) => p[0] >= x0);
  function move(ev) {
    const r = svg.getBoundingClientRect();
    const px = (ev.clientX - r.left) / r.width;   // 0..1 across svg width
    const idx = Math.round(x0 + px * span);
    // nearest commit index that has data
    let best = null, bestd = 1e9;
    for (const [, p] of all) { const dd = Math.abs(p[0] - idx); if (dd < bestd) { bestd = dd; best = p[0]; } }
    if (best == null) return;
    const bx = PADL + (best - x0) / span * (W - PADL - PADR);
    cross.setAttribute('x1', bx); cross.setAttribute('x2', bx); cross.style.opacity = 1;
    const bp = b.base.find(p => p[0] === best), jp = b.jit.find(p => p[0] === best);
    const cm = commits[best];
    let rows = '';
    if (bp) rows += `interp ${fmt(yval(bp))}${unit()}`;
    if (jp) rows += `${bp ? '  ' : ''}+jit ${fmt(yval(jp))}${unit()}`;
    tip.innerHTML = `<span class="mono">${cm.sha}</span> <span class="r">${cm.date.slice(5,10)}</span><br><span class="mono">${rows}</span>`;
    tip.style.left = (bx / W * 100) + '%';
    tip.style.top = '48px';
    tip.style.opacity = 1;
  }
  svg.addEventListener('pointermove', move);
  svg.addEventListener('pointerleave', () => { tip.style.opacity = 0; cross.style.opacity = 0; });
}

let sortKey = 'name', sortDir = 1;
function renderTable() {
  const wrap = document.getElementById('tableWrap');
  const rows = benches.map(b => {
    const dj = delta(b.jit), db = delta(b.base);
    const lastB = b.base.length ? yval(b.base[b.base.length - 1]) : null;
    const lastJ = b.jit.length ? yval(b.jit[b.jit.length - 1]) : null;
    return { name: b.name, base: lastB, jit: lastJ,
             dpct: (dj || db) ? (dj || db).pct : null };
  });
  rows.sort((a, x) => {
    const av = a[sortKey], xv = x[sortKey];
    if (av == null) return 1; if (xv == null) return -1;
    return (av > xv ? 1 : av < xv ? -1 : 0) * sortDir;
  });
  const th = (k, label) => `<th data-k="${k}">${label}${sortKey===k?(sortDir>0?' ↑':' ↓'):''}</th>`;
  let h = `<table><thead><tr>${th('name','benchmark')}${th('base','interp '+unit())}${th('jit','+jit '+unit())}${th('dpct','Δ latest')}</tr></thead><tbody>`;
  for (const r of rows) {
    const dc = r.dpct == null ? 'flat' : Math.abs(r.dpct) < 0.5 ? 'flat' : r.dpct > 0 ? 'up' : 'down';
    const dtxt = r.dpct == null ? '—' : (r.dpct > 0 ? '+' : '') + r.dpct.toFixed(1) + '%';
    h += `<tr><td>${r.name}</td><td class="mono">${r.base==null?'—':fmt(r.base)}</td><td class="mono">${r.jit==null?'—':fmt(r.jit)}</td><td class="mono" style="color:var(--${dc==='up'?'bad':dc==='down'?'good':'muted'})">${dtxt}</td></tr>`;
  }
  h += '</tbody></table>';
  wrap.innerHTML = h;
  wrap.querySelectorAll('th').forEach(t => t.onclick = () => {
    const k = t.dataset.k; if (k === sortKey) sortDir *= -1; else { sortKey = k; sortDir = k === 'name' ? 1 : -1; }
    renderTable();
  });
}

function render() {
  document.getElementById('grid').classList.toggle('hidden', view !== 'charts');
  document.getElementById('tableWrap').classList.toggle('hidden', view !== 'table');
  if (view === 'charts') renderCharts(); else renderTable();
}

function seg(id, cb) {
  const el = document.getElementById(id);
  el.addEventListener('click', e => {
    const btn = e.target.closest('button'); if (!btn) return;
    [...el.children].forEach(b => b.setAttribute('aria-pressed', b === btn));
    cb(btn.dataset.v); render();
  });
}
seg('metric', v => metric = v);
seg('window', v => windowN = +v);
seg('view', v => view = v);

const last = commits[N - 1], first = commits[0];
document.getElementById('meta').textContent =
  `${N} commits · ${first.date.slice(0,10)} → ${last.date.slice(0,10)} · latest ${last.sha}`;
document.getElementById('foot').innerHTML =
  `Each chart has an independent y-axis (small multiples). Values are the median of 7 runs; ` +
  `ratio is mutsu ÷ Rakudo on the same runner (below 1× = faster than raku). ` +
  `Δ latest = last commit vs the previous in the window. ` +
  `Source: <span class="mono">bench-history.tsv</span> on <span class="mono">bench-data</span>.`;
render();
</script>
"""


# Site chrome (nav + footer), added by --site-chrome. The markup is empty on
# purpose: the real navigation, language switch and credit footer are rendered
# into it by the site's own `assets/i18n.js`, so this page can never drift from
# the other pages' chrome. Without the flag the file stays self-contained (no
# external requests) and simply has no chrome.
CHROME_HEAD = '<link rel="stylesheet" href="assets/site.css">'
CHROME_NAV = '<nav class="site-nav"></nav>'
CHROME_FOOTER = """<footer class="site-footer"></footer>
<script type="module">
  import { renderChrome } from './assets/i18n.js';
  renderChrome('bench');
</script>"""


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("input", nargs="?", help="bench-history.tsv (default: stdin)")
    ap.add_argument("-o", "--output", help="output HTML path (default: stdout)")
    ap.add_argument(
        "--standalone",
        action="store_true",
        help="prepend a DOCTYPE so the file renders in standards mode when opened "
        "directly in a browser (omit when publishing as an Artifact, which adds its own)",
    )
    ap.add_argument(
        "--site-chrome",
        action="store_true",
        help="add the mutsu site's shared nav and footer (pulls in assets/site.css and "
        "assets/i18n.js from the same directory). Use when the output is deployed into "
        "wasm-demo/; omit to keep the file self-contained for offline use.",
    )
    args = ap.parse_args()

    text = open(args.input, encoding="utf-8").read() if args.input else sys.stdin.read()
    rows = parse_rows(text)
    if not rows:
        sys.exit("no usable rows parsed from input")
    model = build_model(rows)
    payload = json.dumps(model, separators=(",", ":"))
    # Guard against a stray </script> in the data breaking the inline block.
    payload = payload.replace("</", "<\\/")
    doc = TEMPLATE.replace("__DATA__", payload)
    doc = (
        doc.replace("__CHROME_HEAD__", CHROME_HEAD if args.site_chrome else "")
        .replace("__CHROME_NAV__", CHROME_NAV if args.site_chrome else "")
        .replace("__CHROME_FOOTER__", CHROME_FOOTER if args.site_chrome else "")
    )
    if args.standalone:
        doc = '<!DOCTYPE html>\n<meta charset="utf-8">\n' + doc

    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            f.write(doc)
        print(f"wrote {args.output} ({len(model['benches'])} benchmarks, "
              f"{len(model['commits'])} commits)", file=sys.stderr)
    else:
        sys.stdout.write(doc)


if __name__ == "__main__":
    main()
