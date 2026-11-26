#!/usr/bin/env python3
"""Render bench-history.tsv into a self-contained HTML trend dashboard.

The bench CI appends one row per (commit, benchmark) to `bench-history.tsv` on
the `bench-data` branch, but the raw TSV makes the *trend* hard to read. This
turns it into small-multiple line charts — one mini chart per benchmark, with
the interpreter (MUTSU_JIT=off) and +jit series over commit order — so a
regression or win is visible at a glance. Each card also marks where mutsu
loses against rakudo or rakupp, on a median of the last commits rather than the
latest point, with the issues `benchmarks/bench-tracking.toml` names.

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

Pass `--site-chrome` when the output is deployed into `site/`: it adds the
site's shared nav and footer by loading `assets/site.css` and `assets/i18n.js`
from the same directory, so the dashboard is a page of the site rather than an
orphan. That trades away self-containedness, which is why it is opt-in.
"""

import argparse
import html
import json
import os
import re
import statistics
import sys
import tomllib

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
    # Raku++ (https://github.com/ash/rakupp), the second reference
    # implementation. Appended after `rakudo` for the same reason as
    # `allocations` below: every row written before these columns existed
    # still parses, and simply has no rakupp point.
    "rakupp_median_s",
    "ratio_mutsu_over_rakupp",
    "rakupp",
]


# bench-det-history.tsv (issue #8085): the deterministic series. No raku
# counterpart, no median/min -- an instruction count is exact -- and the
# toolchain rather than the runner, because a simulated count does not depend on
# the runner's CPU but DOES step when the binary's codegen changes.
# `allocations` (#8959) is LAST, after `toolchain`, because it was appended to a
# history that already had thousands of five-field rows: every one of those still
# parses, and simply has no sixth field to read.
DET_COLUMNS = [
    "date",
    "commit",
    "benchmark",
    "instructions",
    "toolchain",
    "allocations",
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
        try:
            rec["ratio_pp"] = float(rec["ratio_mutsu_over_rakupp"])
        except (ValueError, KeyError):
            # Absent before rakupp was measured, NA when its run failed: no
            # point either way.
            rec["ratio_pp"] = None
        rows.append(rec)
    return rows


def parse_det_rows(text):
    """Rows of bench-det-history.tsv. Same shape as `parse_rows`, different
    columns; returns [] for an absent or header-only file so a checkout with no
    deterministic history yet simply renders without that metric."""
    rows = []
    for i, line in enumerate(text.splitlines()):
        if not line.strip():
            continue
        parts = line.split("\t")
        if i == 0 and parts[0] == "date":
            continue  # header
        if len(parts) < 4:
            continue
        rec = dict(zip(DET_COLUMNS, parts))
        try:
            rec["instructions"] = int(rec["instructions"])
        except (ValueError, KeyError):
            continue  # an NA row (the measurement failed)
        try:
            rec["allocations"] = int(rec["allocations"])
        except (ValueError, KeyError):
            # Absent on every row written before the column existed, and NA on a
            # row whose profile could not be read. Either way: no point, rather
            # than a zero that would read as "stopped allocating".
            rec["allocations"] = None
        rows.append(rec)
    return rows


# Per-benchmark caveats, shown on the benchmark's card (and marked in the table)
# only while the named metric is selected: {benchmark: {metric: html}}. The key
# is the benchmark's base name -- `+jit` stripped, `@section` kept -- the same
# name its card is titled with. Use it for a number that is measured correctly
# but does not mean what the chart next to it suggests.
_RAKUPP_NATIVE_JSON = (
    "rakupp does not run JSON::Fast's Raku code: it answers "
    "<span class=\"mono\">JSON::Fast::from-json</span> with its own C++ JSON "
    "parser (the module file is loaded, but a <span class=\"mono\">die</span> "
    "put at the top of <span class=\"mono\">from-json</span> never fires). This "
    "ratio is mutsu interpreting JSON::Fast against a native parser, not engine "
    "against engine; <b>ratio vs rakudo</b>, which runs the same Raku code, is "
    "the comparable number."
)
_RAKUPP_NON_PIE_STARTUP = (
    "Most of this row is process startup, and rakupp is a non-PIE executable "
    "(loaded at a fixed address, no relative relocations). mutsu is a PIE, so "
    "the dynamic loader applies ~80k relocations and copy-on-write faults in "
    "~400 pages first. Linked non-PIE, the same mutsu build measures about "
    "<b>1.1x</b> rakupp here, not ~1.3-1.4x. That trade (less ASLR) is #11158's "
    "call. See <span class=\"mono\">docs/rakupp-gap-survey.md</span>."
)
# Both notes describe a ratio where rakupp wins by something other than running
# the same Raku code faster (a native parser; a non-PIE link), so mutsu being
# slower there is not a gap to close. `engine: False` keeps such a ratio from
# being coloured or counted as a loss: the card shows the note itself instead,
# and `--report` lists the row in its own group, labelled with the short `tag`.
_NOTE_NATIVE_JSON = {"tag": "rakupp parses JSON natively", "engine": False,
                     "html": _RAKUPP_NATIVE_JSON}
_NOTE_NON_PIE = {"tag": "mostly startup; rakupp is non-PIE", "engine": False,
                 "html": _RAKUPP_NON_PIE_STARTUP}
BENCH_NOTES = {
    "bench-json-fast": {"ratiopp": _NOTE_NATIVE_JSON},
    "bench-json-fast-spdx": {"ratiopp": _NOTE_NATIVE_JSON},
    "bench-json-fast-spdx@section": {"ratiopp": _NOTE_NATIVE_JSON},
    "bench-startup": {"ratiopp": _NOTE_NON_PIE},
    "string-concat": {"ratiopp": _NOTE_NON_PIE},
}

# How many of a series' newest points the robust figures read. The bench CI
# records two or three commits a day and wall clock moves 16-33% between runs
# of one binary (docs/benchmarks.md), so a single point is noise; a median of
# ten is about four days of history, short enough to follow a real step (the
# 2026-11-22 bench-threads drop from ~2.5x to ~0.35x rakudo shows up within
# five commits) and long enough that one bad runner does not move it.
RECENT = 10
# A series whose newest point is older than this many commits is no longer
# measured (a renamed or retired benchmark): it has no standing.
STALE_AFTER = 30

DEFAULT_TRACKING = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", "benchmarks", "bench-tracking.toml")
_TRACKING_KEYS = {"issues", "survey", "note"}


def load_tracking(path):
    """benchmarks/bench-tracking.toml: {benchmark: {issues, survey?, note?}}.

    The file is repository data, but the page shows it, so its shape is checked
    here rather than trusted: issue numbers are ints, the survey anchor is a
    slug, the note is plain text. A malformed file fails the render instead of
    putting markup or a broken link on the page."""
    with open(path, "rb") as f:
        raw = tomllib.load(f)
    out = {}
    for name, ent in raw.items():
        if not isinstance(ent, dict):
            raise SystemExit(f"bench-visualize: {path}: [{name}] is not a table")
        extra = set(ent) - _TRACKING_KEYS
        if extra:
            raise SystemExit(f"bench-visualize: {path}: [{name}] has unknown key(s) "
                             + ", ".join(sorted(extra)))
        issues = ent.get("issues", [])
        if not (isinstance(issues, list) and issues
                and all(isinstance(n, int) and not isinstance(n, bool) and n > 0
                        for n in issues)):
            raise SystemExit(f"bench-visualize: {path}: [{name}] needs `issues`, "
                             "a non-empty list of issue numbers")
        rec = {"issues": issues}
        survey = ent.get("survey")
        if survey is not None:
            if not (isinstance(survey, str) and re.fullmatch(r"[a-z0-9-]+", survey)):
                raise SystemExit(f"bench-visualize: {path}: [{name}] survey must be "
                                 "a lowercase anchor slug")
            rec["survey"] = survey
        note = ent.get("note")
        if note is not None:
            if not isinstance(note, str) or re.search(r"[<>&]", note):
                raise SystemExit(f"bench-visualize: {path}: [{name}] note must be "
                                 "plain text (no <, > or &)")
            rec["note"] = note
        out[name] = rec
    return out


def _quantile(sorted_vals, q):
    """Linear-interpolated quantile of an already sorted, non-empty list."""
    pos = (len(sorted_vals) - 1) * q
    lo = int(pos)
    hi = min(lo + 1, len(sorted_vals) - 1)
    return sorted_vals[lo] + (sorted_vals[hi] - sorted_vals[lo]) * (pos - lo)


def robust(values):
    """Median and interquartile range of the newest RECENT values, or None."""
    vals = sorted(values[-RECENT:])
    if not vals:
        return None
    return {"med": round(statistics.median(vals), 4), "lo": round(_quantile(vals, .25), 4),
            "hi": round(_quantile(vals, .75), 4), "n": len(vals)}


def standing(entry, n_commits):
    """Where mutsu stands against each reference on this benchmark, read from
    the shipped configuration's series (+jit, the default) and falling back to
    the interpreter series for a benchmark recorded only without it.

    Each ratio is the median of the per-commit ratios over the newest RECENT
    points -- not the latest point -- with its interquartile range, so a row is
    "losing" because it has been slower for days, and "marginal" when a quarter
    or more of those points were at or under parity anyway."""
    lane = "jit" if entry["jit"] else "base"
    pts = entry[lane]
    if not pts or pts[-1][0] < n_commits - STALE_AFTER:
        return None
    out = {"lane": lane}
    for key, slot in (("ratio", 2), ("ratiopp", 5)):
        r = robust([p[slot] for p in pts if p[slot] is not None])
        if r is not None:
            r["losing"] = r["med"] > 1.0
            r["marginal"] = r["losing"] and r["lo"] <= 1.0
        out[key] = r
    return out


def losers(benches):
    """The benchmarks mutsu loses, worst first.

    Ranked by the worst engine-against-engine ratio. A row that loses only on a
    ratio whose note says it is not engine against engine (rakupp's native JSON
    parser) is still listed -- labelled, after the ranked rows -- rather than
    hidden or ranked on a number that does not compare engines."""
    ranked, other = [], []
    for b in benches:
        st = b.get("standing")
        if not st:
            continue
        engine, any_loss = [], []
        for key in ("ratio", "ratiopp"):
            r = st.get(key)
            if not r or not r["losing"]:
                continue
            any_loss.append(r["med"])
            note = b.get("notes", {}).get(key)
            if not note or note["engine"]:
                engine.append(r["med"])
        if engine:
            ranked.append((-max(engine), b["name"]))
        elif any_loss:
            other.append((-max(any_loss), b["name"]))
    return {"ranked": [n for _, n in sorted(ranked)],
            "other": [n for _, n in sorted(other)]}


def build_model(rows, det_rows=(), tracking=None):
    # Chronological commit order: each distinct commit keyed by first-seen date.
    # Both series are appended by the same job on the same commit, so they share
    # this axis -- but build it from BOTH, so a commit recorded in only one of
    # them still gets a column instead of silently dropping the other's point.
    first_seen = {}
    for r in list(rows) + list(det_rows):
        c = r["commit"]
        if c not in first_seen or r["date"] < first_seen[c]:
            first_seen[c] = r["date"]
    order = sorted(first_seen, key=lambda c: (first_seen[c], c))
    idx = {c: i for i, c in enumerate(order)}
    commit_meta = [{"sha": c[:9], "date": first_seen[c]} for c in order]

    def split_lane(name):
        """`foo+jit` -> ("foo", "jit"); `foo` -> ("foo", "base")."""
        return (name[:-4], "jit") if name.endswith("+jit") else (name, "base")

    # The deterministic row for each (benchmark, commit), keyed the same way, so
    # it can be attached to the matching wall-clock point below.
    det = {}
    for r in det_rows:
        det[(r["benchmark"], r["commit"])] = (r["instructions"], r["allocations"])

    # benchmark basename -> {"base": [...pts], "jit": [...pts]}
    # A point is [commitIdx, seconds, ratio, instructions|None, allocations|None,
    # ratio_vs_rakupp|None] -- the instruction/allocation slots are None
    # wherever the deterministic series has no row, and the rakupp slot wherever
    # rakupp was not measured, which is every commit before each of them
    # started being recorded.
    benches = {}
    for r in rows:
        name = r["benchmark"]
        base_name, lane = split_lane(name)
        b = benches.setdefault(base_name, {"base": {}, "jit": {}})
        instr, allocs = det.pop((name, r["commit"]), (None, None))
        # last write wins if a commit reran a benchmark
        b[lane][idx[r["commit"]]] = [
            idx[r["commit"]],
            round(r["mutsu_median_s"], 5),
            round(r["ratio"], 4),
            instr,
            allocs,
            None if r["ratio_pp"] is None else round(r["ratio_pp"], 4),
        ]

    # Any deterministic row with no wall-clock twin (the wall-clock measurement
    # failed, or the benchmark is recorded in only one of the two) still gets a
    # point, with the seconds/ratio slots empty.
    for (name, commit), (instr, allocs) in det.items():
        base_name, lane = split_lane(name)
        b = benches.setdefault(base_name, {"base": {}, "jit": {}})
        b[lane].setdefault(idx[commit], [idx[commit], None, None, instr, allocs, None])

    out = []
    for name in sorted(benches):
        base = [benches[name]["base"][k] for k in sorted(benches[name]["base"])]
        jit = [benches[name]["jit"][k] for k in sorted(benches[name]["jit"])]
        entry = {"name": name, "base": base, "jit": jit}
        if name in BENCH_NOTES:
            entry["notes"] = BENCH_NOTES[name]
        st = standing(entry, len(order))
        if st:
            entry["standing"] = st
        if tracking and name in tracking:
            entry["tracking"] = tracking[name]
        out.append(entry)
    return {
        "commits": commit_meta,
        "benches": out,
        "losers": losers(out),
        "recent": RECENT,
        # When the newest measurement was recorded -- the latest row date across
        # both series, not the last commit's first-seen date, which a re-run of an
        # already-recorded commit would leave behind. Shown in the page header so
        # a stale deploy is visible at a glance.
        "updated": max(r["date"] for r in list(rows) + list(det_rows)),
        # Drives whether the "instructions" metric button is offered at all: a
        # checkout whose bench-data has no deterministic history yet would
        # otherwise show a button that renders nothing.
        "hasDet": bool(det_rows),
        # Same, for "allocations" (#8959), which started being recorded later
        # than the instruction counts and so is absent from the older rows.
        "hasAllocs": any(r["allocations"] is not None for r in det_rows),
        # Same, for "ratio vs rakupp", which started being recorded later still.
        "hasRakupp": any(r["ratio_pp"] is not None for r in rows),
    }


TEMPLATE = r"""<title>Benchmark trend &mdash; mutsu</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
__CHROME_HEAD__
<style>
/* Palette shared with the rest of the site (site/assets/site.css), so the
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
.bench-note {
  margin: 2px 0 6px; padding: 6px 8px; font-size: 11.5px; line-height: 1.45;
  color: var(--ink); border-left: 3px solid var(--jit); border-radius: 4px;
  background: color-mix(in srgb, var(--jit) 10%, transparent);
}
.note-mark { color: var(--jit); cursor: help; margin-left: 4px; }
table { border-collapse: collapse; width: 100%; font-size: 12.5px; }
th, td { text-align: right; padding: 6px 10px; border-bottom: 1px solid var(--hair); }
th:first-child, td:first-child { text-align: left; }
thead th { position: sticky; top: 0; background: var(--card); color: var(--muted);
  font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: .05em; cursor: pointer; }
tbody tr:hover { background: color-mix(in srgb, var(--ink) 8%, transparent); }
.hidden { display: none; }
/* Wins and losses against a reference engine, wherever a ratio is shown. A
   marginal loss (a quarter of the recent points at or under parity) keeps the
   colour but drops the weight. A caveated ratio is not coloured: its card
   carries the note instead. */
.lose { color: var(--bad); font-weight: 650; }
.lose.marginal { font-weight: 400; }
.standing { display: flex; gap: 12px; flex-wrap: wrap; align-items: baseline;
  font-size: 11.5px; color: var(--muted); margin: 0 0 4px; }
.standing a { color: var(--muted); }
.standing a:hover { color: var(--ink); }
.foot { color: var(--muted); font-size: 11.5px; margin-top: 22px; }
</style>
__CHROME_NAV__
<div class="bench-head">
  <div class="title">
    <h1>Benchmark trend</h1>
    <span class="sub" id="meta"></span>
    <time class="sub" id="updated"></time>
  </div>
  <div class="controls">
    <span class="ctl-label">Metric</span>
    <div class="seg" id="metric" role="group" aria-label="Metric">
      <button data-v="seconds" aria-pressed="true">mutsu seconds</button>
      <button data-v="ratio" aria-pressed="false">ratio vs rakudo</button>
      <button data-v="ratiopp" aria-pressed="false" id="metricRakupp" hidden>ratio vs rakupp</button>
      <button data-v="instr" aria-pressed="false" id="metricInstr" hidden>instructions</button>
      <button data-v="allocs" aria-pressed="false" id="metricAllocs" hidden>allocations</button>
    </div>
    <span class="ctl-label">Window</span>
    <div class="seg" id="window" role="group" aria-label="Commit window">
      <button data-v="50" aria-pressed="false">50</button>
      <button data-v="150" aria-pressed="true">150</button>
      <button data-v="0" aria-pressed="false">all</button>
    </div>
    <span class="ctl-label" id="yaxisLabel">Y axis</span>
    <div class="seg" id="yaxis" role="group" aria-label="Y axis origin">
      <button data-v="fit" aria-pressed="true">fit</button>
      <button data-v="zero" aria-pressed="false">from 0</button>
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
let metric = 'seconds', windowN = 150, view = 'charts', yaxis = 'fit';
let sortKey = 'name', sortDir = 1;

// A point is [commitIdx, seconds, ratio, instructions, allocations,
// ratio_vs_rakupp]. The last three are null for every commit before each series
// started being recorded, so every consumer below has to tolerate a null y --
// see `defined`.
const YIDX = { seconds: 1, ratio: 2, instr: 3, allocs: 4, ratiopp: 5 };
const isRatio = () => metric === 'ratio' || metric === 'ratiopp';
const yval = (pt) => pt[YIDX[metric]];
const defined = (pt) => yval(pt) != null;
// Instruction counts run to 1e9-1e13 and allocation counts to 1e4-1e8, so both
// get SI suffixes rather than the seconds formatter's fixed decimals.
function fmtInstr(v) {
  const a = Math.abs(v);
  if (a >= 1e12) return (v / 1e12).toFixed(2) + 'T';
  if (a >= 1e9) return (v / 1e9).toFixed(2) + 'G';
  if (a >= 1e6) return (v / 1e6).toFixed(1) + 'M';
  if (a >= 1e3) return (v / 1e3).toFixed(1) + 'k';
  return String(v);
}
const fmt = (v) => (metric === 'instr' || metric === 'allocs') ? fmtInstr(v)
  : metric === 'seconds'
    ? (v === 0 ? '0' : v < 0.001 ? v.toExponential(1) : v.toFixed(v < 0.1 ? 4 : 3))
    : v.toFixed(2);
const unit = () => metric === 'seconds' ? 's' : isRatio() ? '×' : '';

// A caveat attached to this benchmark for the active metric (BENCH_NOTES in
// the generator), or undefined.
const noteFor = (b) => b.notes && b.notes[metric] && b.notes[metric].html;
// The note a card shows: the active metric's, or else the note of a ratio its
// "vs rakudo / vs rakupp" line shows in every metric -- that ratio needs its
// explanation next to it whatever is selected.
const cardNoteFor = (b) => noteFor(b)
  || (b.notes && b.standing && REFS.map(([k]) => b.standing[k] && b.notes[k] && b.notes[k].html)
        .find(Boolean));
// A caveated ratio (rakupp answering natively, its non-PIE startup) is not a
// loss to act on: it is shown uncoloured, with the note on the card.
const caveated = (b, k) => !!(b.notes && b.notes[k] && b.notes[k].engine === false);
const RECENT = DATA.recent;
const ISSUE_URL = 'https://github.com/tokuhirom/mutsu/issues/';
const REFS = [['ratio', 'rakudo'], ['ratiopp', 'rakupp']];
const median = (xs) => {
  const v = [...xs].sort((a, b) => a - b), m = v.length >> 1;
  return v.length % 2 ? v[m] : (v[m - 1] + v[m]) / 2;
};

// Build a DOM node. Text goes in through `append` (a text node), never as
// markup, so that nothing read from the data is ever parsed as HTML.
function el(tag, attrs, ...kids) {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs || {})) {
    if (v == null) continue;
    if (k === 'class') e.className = v; else e.setAttribute(k, v);
  }
  for (const k of kids) if (k != null) e.append(k);
  return e;
}
const stripTags = (h) => h.replace(/<[^>]*>/g, '');
// One robust ratio (`standing` in the generator): its colour class and the
// tooltip that says what the number is.
const ratioClass = (r) => !r ? '' : r.losing ? (r.marginal ? 'lose marginal' : 'lose') : 'win';
const ratioTitle = (r) => `median of the last ${r.n} commits; interquartile range `
  + `${r.lo.toFixed(2)}-${r.hi.toFixed(2)}×`
  + (r.marginal ? '; a quarter or more of them at or under 1.0×' : '');
function ratioCell(b, key) {
  const r = b.standing && b.standing[key];
  if (!r) return el('td', { class: 'mono' }, '—');
  return el('td', { class: 'mono ' + (caveated(b, key) ? '' : ratioClass(r)), title: ratioTitle(r) },
    r.med.toFixed(2) + '×');
}

function windowStart() { return windowN > 0 ? Math.max(0, N - windowN) : 0; }

// The newest RECENT points of a series, as values. Deliberately not limited to
// the commit window: the window shapes the charts, but a median or a trend has
// to read the same points whatever is zoomed in -- and a 50- or 150-commit
// window can hold fewer than 2 x RECENT wall-clock points, since the
// deterministic series records more commits than the wall-clock one.
function recentVals(pts) {
  return pts.filter(defined).slice(-RECENT).map(yval);
}

// A trend that a single noisy commit cannot fake: the median of the newest
// RECENT points against the median of the RECENT before them, reported only
// when the step is larger than the noise. The noise is the series' own: the
// spread of each half around its median (MAD, scaled to a standard deviation),
// turned into the standard error of a difference of two medians (1.2533 s
// sqrt(2/k)). A step beyond two of those is shown; anything smaller -- which on
// a 16-33%-noisy wall-clock series is most commit-to-commit motion -- is not.
// The floor (0.5%) is for the deterministic series, whose spread can be zero.
function trend(pts) {
  const w = pts.filter(defined).map(yval);
  if (w.length < 2 * RECENT) return null;
  const cur = w.slice(-RECENT), prev = w.slice(-2 * RECENT, -RECENT);
  const mc = median(cur), mp = median(prev);
  if (!mp) return null;
  const resid = [...cur.map(v => Math.abs(v - mc)), ...prev.map(v => Math.abs(v - mp))];
  const sigma = 1.4826 * median(resid);
  const band = Math.max(2 * 1.2533 * sigma * Math.sqrt(2 / RECENT), 0.005 * Math.abs(mp));
  return { pct: (mc - mp) / mp * 100, bandPct: band / Math.abs(mp) * 100,
           real: Math.abs(mc - mp) > band };
}

function trendChip(t) {
  if (!t) return `<span class="chip flat" title="fewer than ${2 * RECENT} points recorded">—</span>`;
  const band = `±${t.bandPct.toFixed(1)}%`;
  if (!t.real) return `<span class="chip flat" title="median of the last ${RECENT} vs the ${RECENT} before moved ${t.pct.toFixed(1)}%, inside the noise band ${band}">no change</span>`;
  const word = metric === 'seconds' ? (t.pct > 0 ? ' slower' : ' faster')
    : (metric === 'instr' || metric === 'allocs') ? (t.pct > 0 ? ' more' : ' fewer') : '';
  return `<span class="chip ${t.pct > 0 ? 'up' : 'down'}" title="median of the last ${RECENT} vs the ${RECENT} before; noise band ${band}">${t.pct > 0 ? '+' : ''}${t.pct.toFixed(1)}%${word}</span>`;
}

const W = 300, H = 96, PADL = 6, PADR = 10, PADT = 10, PADB = 16;

function chart(b) {
  const x0 = windowStart(), span = Math.max(1, (N - 1) - x0);
  const sx = (i) => PADL + (i - x0) / span * (W - PADL - PADR);
  const lanes = [['base', b.base], ['jit', b.jit]]
    .map(([k, pts]) => [k, pts.filter(p => p[0] >= x0 && defined(p))]);
  let lo = Infinity, hi = -Infinity;
  for (const [, pts] of lanes) for (const p of pts) {
    const v = yval(p); if (v < lo) lo = v; if (v > hi) hi = v;
  }
  if (isRatio()) { lo = Math.min(lo, 1); hi = Math.max(hi, 1); }
  if (!isFinite(lo)) { lo = 0; hi = 1; }
  // `zero` anchors the axis at 0 so a chart shows a change's size relative to
  // the whole value; `fit` zooms onto the data's own range so small moves show.
  if (yaxis === 'zero') { lo = Math.min(lo, 0); hi = Math.max(hi, 0); }
  if (hi === lo) { hi = lo + 1; }
  const pad = (hi - lo) * 0.12;
  if (yaxis !== 'zero' || lo < 0) lo -= pad;
  if (yaxis !== 'zero' || hi > 0) hi += pad;
  const sy = (v) => PADT + (1 - (v - lo) / (hi - lo)) * (H - PADT - PADB);

  let svg = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="${b.name} trend">`;
  for (const t of [0.5]) { const y = PADT + t * (H - PADT - PADB);
    svg += `<line class="gridline" x1="${PADL}" x2="${W - PADR}" y1="${y}" y2="${y}"/>`; }
  if (isRatio() && 1 >= lo && 1 <= hi) {
    const y = sy(1);
    svg += `<line class="refline" x1="${PADL}" x2="${W - PADR}" y1="${y}" y2="${y}"/>`;
    svg += `<text class="axis" x="${W - PADR}" y="${y - 3}" text-anchor="end">${metric === 'ratiopp' ? 'rakupp' : 'rakudo'}</text>`;
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
  // The shipped configuration (+jit) when it has points in the window.
  const nowLane = recentVals(b.jit).length ? 'jit' : 'base';
  const vals = recentVals(b[nowLane]);
  const now = vals.length
    ? `<span class="now mono" title="median of the last ${vals.length} points">${fmt(median(vals))}<span class="unit">${unit()}${nowLane==='jit'?' (+jit)':''}</span></span>`
    : '';
  const chip = trendChip(trend(b[nowLane]));
  // Where mutsu stands against each reference, whatever metric is shown.
  const st = b.standing || {};
  const esc = (t) => t.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
  let standingLine = REFS.filter(([k]) => st[k]).map(([k, ref]) =>
    `<span title="${ratioTitle(st[k])}">vs ${ref} <b class="mono ${caveated(b, k) ? '' : ratioClass(st[k])}">${st[k].med.toFixed(2)}×</b></span>`
  ).join('');
  // A losing card names the issues that track the gap (bench-tracking.toml;
  // numbers only, validated by the generator), quietly, after its ratios.
  const losing = REFS.some(([k]) => st[k] && st[k].losing && !caveated(b, k));
  if (losing && b.tracking) {
    standingLine += `<span title="${b.tracking.note ? esc(b.tracking.note) : 'tracking issues'}">`
      + b.tracking.issues.map(n => `<a href="${ISSUE_URL}${n}" rel="noopener">#${n}</a>`).join(' ')
      + '</span>';
  }
  const note = cardNoteFor(b);
  return `<div class="bench-card" data-name="${b.name}">
    <div class="head"><h2>${b.name}</h2></div>
    <div class="readout">${now}${chip}</div>
    ${standingLine ? `<div class="standing">${standingLine}</div>` : ''}
    ${note ? `<p class="bench-note">${note}</p>` : ''}
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
  const all = [...b.base.map(p => ['base', p]), ...b.jit.map(p => ['jit', p])]
    .filter(([, p]) => p[0] >= x0 && defined(p));
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
    const bp = b.base.find(p => p[0] === best && defined(p));
    const jp = b.jit.find(p => p[0] === best && defined(p));
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

function renderTable() {
  const wrap = document.getElementById('tableWrap');
  const rows = benches.map(b => {
    const vb = recentVals(b.base), vj = recentVals(b.jit);
    const t = trend(vj.length ? b.jit : b.base);
    const st = b.standing || {};
    return { b, name: b.name, base: vb.length ? median(vb) : null,
             jit: vj.length ? median(vj) : null, note: cardNoteFor(b),
             // Only a trend beyond the noise sorts as one; the rest is 0.
             trend: t ? (t.real ? t.pct : 0) : null, t,
             rk: st.ratio ? st.ratio.med : null, pp: st.ratiopp ? st.ratiopp.med : null };
  });
  rows.sort((a, x) => {
    const av = a[sortKey], xv = x[sortKey];
    if (av == null) return 1; if (xv == null) return -1;
    return (av > xv ? 1 : av < xv ? -1 : 0) * sortDir;
  });
  const th = (k, label) => `<th data-k="${k}">${label}${sortKey===k?(sortDir>0?' ↑':' ↓'):''}</th>`;
  let h = `<table><thead><tr>${th('name','benchmark')}${th('base','interp '+unit())}${th('jit','+jit '+unit())}${th('trend','trend')}${th('rk','vs rakudo')}${th('pp','vs rakupp')}</tr></thead><tbody></tbody></table>`;
  wrap.innerHTML = h;
  const body = wrap.querySelector('tbody');
  for (const r of rows) {
    // The table has no room for the note itself: mark the row and carry the
    // note's text as a tooltip.
    const mark = r.note ? el('span', { class: 'note-mark', title: stripTags(r.note) }, '†') : null;
    const tc = document.createElement('td');
    tc.className = 'mono';
    tc.innerHTML = trendChip(r.t);
    body.append(el('tr', {},
      el('td', {}, r.name, mark),
      el('td', { class: 'mono' }, r.base == null ? '—' : fmt(r.base)),
      el('td', { class: 'mono' }, r.jit == null ? '—' : fmt(r.jit)),
      tc, ratioCell(r.b, 'ratio'), ratioCell(r.b, 'ratiopp')));
  }
  wrap.querySelectorAll('th').forEach(t => t.onclick = () => {
    const k = t.dataset.k; if (k === sortKey) sortDir *= -1; else { sortKey = k; sortDir = k === 'name' ? 1 : -1; }
    writeHash();
    renderTable();
  });
}

function render() {
  document.getElementById('grid').classList.toggle('hidden', view !== 'charts');
  document.getElementById('tableWrap').classList.toggle('hidden', view !== 'table');
  if (view === 'charts') renderCharts(); else renderTable();
}

/* ---- URL state ----------------------------------------------------------
   The controls above are the page's entire state, so they belong in the URL:
   a link to "instructions, last 150 commits, as a table" has to reopen exactly
   that, and a reload must not silently throw the reader's selection away.
   `#metric=instr&window=50&y=zero` or `#view=table&sort=jit&dir=desc`, with defaults left
   out so an untouched page keeps a bare URL. `replaceState` rather than
   assigning to `location.hash`: clicking through four metrics is one page, not
   four history entries to back out of. */
const DEFAULTS = { metric: 'seconds', window: '150', view: 'charts', y: 'fit', sort: 'name', dir: 'asc' };
const VALID = {
  // `instr` and `allocs` are only offered when the series behind them exists, so
  // a stale link to one must fall back rather than render an empty page. Keep
  // this list in step with the buttons in the `metric` group: a metric that has
  // a button but is missing here cannot be linked to or survive a reload, which
  // is silent, and is the shape #8959's `allocs` was in when it met this
  // validator. The generator asserts the two agree (see `_assert_metrics_valid`).
  metric: (v) => v === 'seconds' || v === 'ratio'
    || (v === 'instr' && DATA.hasDet) || (v === 'allocs' && DATA.hasAllocs)
    || (v === 'ratiopp' && DATA.hasRakupp),
  window: (v) => v === '0' || v === '50' || v === '150',
  view: (v) => v === 'charts' || v === 'table',
  y: (v) => v === 'fit' || v === 'zero',
  // Keep in step with the table's `th(...)` keys; asserted like the metrics.
  sort: (v) => v === 'name' || v === 'base' || v === 'jit' || v === 'trend'
    || v === 'rk' || v === 'pp',
  dir: (v) => v === 'asc' || v === 'desc',
};

function currentState() {
  const st = { metric, window: String(windowN), view, y: yaxis,
               sort: sortKey, dir: sortDir > 0 ? 'asc' : 'desc' };
  // The sort only exists in the table view; carrying it around in the charts
  // URL would be noise a reader cannot act on.
  if (view !== 'table') { delete st.sort; delete st.dir; }
  // Likewise the y-axis origin only shapes the charts.
  else delete st.y;
  return st;
}

function stateToHash() {
  return Object.entries(currentState())
    .filter(([k, v]) => v !== DEFAULTS[k])
    .map(([k, v]) => k + '=' + encodeURIComponent(v))
    .join('&');
}

// Our own writes must not look like a reader editing the URL, so remember what
// we last wrote and ignore the `hashchange` it may produce.
let lastHash = location.hash.replace(/^#/, '');

function writeHash() {
  lastHash = stateToHash();
  // A `file://` copy (the point of --standalone) is an opaque origin in some
  // browsers, where replaceState throws. The dashboard still works there; only
  // the shareable URL does not, so this must never take the click handler with
  // it.
  try {
    history.replaceState(null, '',
      location.pathname + location.search + (lastHash ? '#' + lastHash : ''));
  } catch (e) { /* no history API here: the controls still work */ }
}

function applyHash() {
  const q = {};
  for (const part of location.hash.replace(/^#/, '').split('&')) {
    const i = part.indexOf('=');
    if (i > 0) q[decodeURIComponent(part.slice(0, i))] = decodeURIComponent(part.slice(i + 1));
  }
  const pick = (k) => (q[k] != null && VALID[k](q[k])) ? q[k] : DEFAULTS[k];
  metric = pick('metric');
  windowN = +pick('window');
  view = pick('view');
  yaxis = pick('y');
  sortKey = pick('sort');
  sortDir = pick('dir') === 'desc' ? -1 : 1;
  syncButtons();
}

function syncButtons() {
  const press = (id, v) => [...document.getElementById(id).children]
    .forEach(b => b.setAttribute('aria-pressed', String(b.dataset.v === v)));
  press('metric', metric);
  press('window', String(windowN));
  press('view', view);
  press('yaxis', yaxis);
  // The origin choice has nothing to act on in the table view.
  document.getElementById('yaxis').classList.toggle('hidden', view !== 'charts');
  document.getElementById('yaxisLabel').classList.toggle('hidden', view !== 'charts');
}

// Adopt whatever the URL says, then normalize it: a hash that named something
// impossible (`instr` with no deterministic series, a junk window) is ignored
// above, and leaving it in the URL would describe a page that is not on screen.
function adoptHash() {
  applyHash();
  if (stateToHash() !== lastHash) writeHash();
}

function seg(id, cb) {
  const el = document.getElementById(id);
  el.addEventListener('click', e => {
    const btn = e.target.closest('button'); if (!btn) return;
    cb(btn.dataset.v); syncButtons(); writeHash(); render();
  });
}
if (DATA.hasDet) document.getElementById('metricInstr').hidden = false;
if (DATA.hasAllocs) document.getElementById('metricAllocs').hidden = false;
if (DATA.hasRakupp) document.getElementById('metricRakupp').hidden = false;
seg('metric', v => metric = v);
seg('window', v => windowN = +v);
seg('view', v => view = v);
seg('yaxis', v => yaxis = v);

// A hand-edited URL, or a back/forward step between two states, re-renders.
window.addEventListener('hashchange', () => {
  if (location.hash.replace(/^#/, '') === lastHash) return;
  lastHash = location.hash.replace(/^#/, '');
  adoptHash();
  render();
});

adoptHash();

const last = commits[N - 1], first = commits[0];
document.getElementById('meta').textContent =
  `${N} commits · ${first.date.slice(0,10)} → ${last.date.slice(0,10)} · latest ${last.sha}`;
// Last-updated stamp: the ISO timestamp is shown in UTC so it matches the
// bench-data rows verbatim; the viewer's local time rides along as a tooltip.
{
  const updated = document.getElementById('updated');
  const at = new Date(DATA.updated);
  updated.textContent = `updated ${DATA.updated.slice(0,10)} ${DATA.updated.slice(11,16)} UTC`;
  updated.setAttribute('datetime', DATA.updated);
  if (!isNaN(at)) updated.title = at.toLocaleString();
}
document.getElementById('foot').innerHTML =
  `Each chart has an independent y-axis (small multiples), fitted to the data by default; ` +
  `<b>Y axis: from 0</b> anchors every axis at zero instead. Values are the median of 7 runs; ` +
  `<b>ratio vs rakudo</b> is mutsu ÷ Rakudo on the same runner (below 1× = faster than Rakudo). ` +
  (DATA.hasRakupp
    ? `<b>ratio vs rakupp</b> is mutsu ÷ <a href="https://github.com/ash/rakupp">Raku++</a>, ` +
      `an independent C++ implementation, measured the same way on the same runner. `
    : '') +
  `The big number on a card and the table's columns are the <b>median of the last ${RECENT} ` +
  `points</b> in the window, not the latest one: a CI runner is a lottery, and one commit's ` +
  `move is mostly noise. <b>trend</b> compares that median with the median of the ${RECENT} ` +
  `points before it, and is shown only when the step is larger than twice the standard ` +
  `error of a difference of medians estimated from the series' own spread ` +
  `(hover a chip for its band); otherwise it reads <i>no change</i>. ` +
  (DATA.hasDet
    ? `<b>instructions</b> is the deterministic series: simulated instruction counts ` +
      `(callgrind), which do not depend on the runner and reproduce to ~0.1%, against ` +
      `16-33% for wall clock. It is blind to cache behaviour, memory boundness, lock ` +
      `contention and real thread parallelism, and it steps whenever the toolchain ` +
      `changes &mdash; read it together with the seconds series, not instead of it. `
    : '') +
  (DATA.hasAllocs
    ? `<b>allocations</b> is the heap allocation count, read out of the same callgrind ` +
      `profile at no extra cost and at least as reproducible as the instruction count ` +
      `(0&ndash;0.01% between warm runs of one binary), and unlike it, barely moved by a ` +
      `toolchain bump. It counts calls, not bytes, so the same number of larger blocks ` +
      `looks identical. `
    : '') +
  `Source: <span class="mono">bench-history.tsv</span>` +
  (DATA.hasDet ? ` and <span class="mono">bench-det-history.tsv</span>` : '') +
  ` on <span class="mono">bench-data</span>. <b>vs rakudo / vs rakupp</b> on a card and in ` +
  `the table are the median of mutsu ÷ the reference over the last ${RECENT} commits; red ` +
  `means mutsu is slower (lighter red: a quarter or more of those commits were at or under ` +
  `parity), and a losing card links its tracking issues from ` +
  `<span class="mono">benchmarks/bench-tracking.toml</span>.`;
render();
</script>
"""


# Site chrome (nav + footer), added by --site-chrome. The markup is empty on
# purpose: the real navigation, language switch and credit footer are rendered
# into it by the site's own `assets/i18n.js`, so this page can never drift from
# the other pages' chrome. Without the flag the file stays self-contained (no
# external requests) and simply has no chrome.
CHROME_HEAD = (
    '<link rel="icon" href="assets/favicon.svg" type="image/svg+xml">\n'
    '<link rel="icon" href="assets/favicon-32.png" sizes="32x32" type="image/png">\n'
    '<link rel="icon" href="assets/favicon-16.png" sizes="16x16" type="image/png">\n'
    '<link rel="apple-touch-icon" href="assets/apple-touch-icon.png">\n'
    '<link rel="stylesheet" href="assets/site.css">'
)
CHROME_NAV = '<nav class="site-nav"></nav>'
CHROME_FOOTER = """<footer class="site-footer"></footer>
<script type="module">
  import { renderChrome } from './assets/i18n.js';
  renderChrome('bench');
</script>"""


def _assert_metrics_valid(doc):
    """Every metric button must be a metric the URL state accepts.

    The two lists live ~230 lines apart in the template -- the buttons in the
    `metric` group, and the `VALID.metric` predicate that decides whether a
    `#metric=...` in the URL is honoured -- and they were added by different
    changes: the URL state (#8963) was written against the file as it stood
    before the `allocations` metric (#8959) existed, so it silently rejected
    `allocs`. The button worked and wrote the hash; reloading that hash threw the
    selection away, which is precisely what the URL state exists to prevent.

    A textual merge cannot catch that, and neither can any test of either change
    on its own, so assert it here: this function runs on every render, which
    includes the pages.yml deploy.
    """
    seg = re.search(r'id="metric"(.*?)</div>', doc, re.S)
    valid = re.search(r"\n  metric: \(v\) =>(.*?),\n  window:", doc, re.S)
    if not seg or not valid:
        raise SystemExit(
            "bench-visualize: cannot find the metric buttons or the VALID.metric "
            "predicate -- the template changed shape; update _assert_metrics_valid."
        )
    buttons = set(re.findall(r'data-v="([^"]+)"', seg.group(1)))
    accepted = set(re.findall(r"v === '([^']+)'", valid.group(1)))
    missing = buttons - accepted
    if missing:
        raise SystemExit(
            "bench-visualize: metric button(s) "
            + ", ".join(sorted(missing))
            + " are not accepted by VALID.metric, so a link to them cannot be "
            "reopened and a reload would fall back to the default. Add them there."
        )
    stale = accepted - buttons
    if stale:
        raise SystemExit(
            "bench-visualize: VALID.metric accepts "
            + ", ".join(sorted(stale))
            + " with no button to select it. Remove it, or add the button."
        )


def report_markdown(model):
    """The loser list as Markdown, for an issue comment or a status report."""
    by = {b["name"]: b for b in model["benches"]}

    def cell(b, key):
        r = b.get("standing", {}).get(key)
        if not r:
            return "-"
        txt = f"{r['med']:.2f}x"
        if r["losing"]:
            txt = f"**{txt}**" if not r["marginal"] else f"{txt} (marginal)"
        note = b.get("notes", {}).get(key)
        return f"{txt} ({note['tag']})" if note else txt

    lines = [f"Median of the last {model['recent']} commits, +jit series.", "",
             "| # | benchmark | vs rakudo | vs rakupp | tracked by |",
             "|---:|---|---:|---:|---|"]
    names = model["losers"]["ranked"] + model["losers"]["other"]
    for i, name in enumerate(names, 1):
        b = by[name]
        tr = b.get("tracking")
        track = ", ".join(f"#{n}" for n in tr["issues"]) if tr else "**none**"
        if tr and tr.get("survey"):
            track += f", survey `#{tr['survey']}`"
        lines.append(f"| {i} | {name} | {cell(b, 'ratio')} | {cell(b, 'ratiopp')} | {track} |")
    return "\n".join(lines)


def _assert_sorts_valid(doc):
    """Every sortable table column must be a sort key the URL state accepts --
    the same drift `_assert_metrics_valid` guards, for the table's `th(...)`."""
    cols = set(re.findall(r"\$\{th\('([a-z]+)'", doc))
    valid = re.search(r"\n  sort: \(v\) =>(.*?),\n  dir:", doc, re.S)
    if not cols or not valid:
        raise SystemExit("bench-visualize: cannot find the table columns or the VALID.sort "
                         "predicate -- the template changed shape; update _assert_sorts_valid.")
    accepted = set(re.findall(r"v === '([^']+)'", valid.group(1)))
    if cols != accepted:
        raise SystemExit("bench-visualize: table sort keys " + ", ".join(sorted(cols))
                         + " differ from VALID.sort " + ", ".join(sorted(accepted)))


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
        "--det",
        metavar="TSV",
        help="bench-det-history.tsv, the deterministic series: instruction counts "
        "(#8085) and heap allocation counts (#8959). Optional: without it the page "
        "renders exactly as before and the `instructions` / `allocations` metric buttons "
        "are not offered, which is what a checkout whose bench-data has no deterministic "
        "history yet needs. A missing file is not an error, for the same reason, and "
        "neither is a history that predates the allocation column -- that button alone "
        "stays hidden.",
    )
    ap.add_argument(
        "--site-chrome",
        action="store_true",
        help="add the mutsu site's shared nav and footer (pulls in assets/site.css and "
        "assets/i18n.js from the same directory). Use when the output is deployed into "
        "site/; omit to keep the file self-contained for offline use.",
    )
    ap.add_argument(
        "--tracking",
        metavar="TOML",
        default=DEFAULT_TRACKING,
        help="which issue tracks each losing benchmark (default: "
        "benchmarks/bench-tracking.toml next to this script)",
    )
    ap.add_argument(
        "--report",
        action="store_true",
        help="print the losing rows, worst first, as a Markdown table on stderr "
        "(stdout when no HTML is written, i.e. with -o and this flag, or this flag alone)",
    )
    args = ap.parse_args()

    text = open(args.input, encoding="utf-8").read() if args.input else sys.stdin.read()
    rows = parse_rows(text)
    if not rows:
        sys.exit("no usable rows parsed from input")
    det_rows = []
    if args.det:
        try:
            det_rows = parse_det_rows(open(args.det, encoding="utf-8").read())
        except OSError as e:
            print(f"bench-visualize: no deterministic series ({e}); "
                  f"rendering wall clock only", file=sys.stderr)
    model = build_model(rows, det_rows, load_tracking(args.tracking))
    for name in model["losers"]["ranked"] + model["losers"]["other"]:
        if not any(b["name"] == name and "tracking" in b for b in model["benches"]):
            print(f"bench-visualize: {name} loses with no tracking issue; file one "
                  f"and add it to {os.path.relpath(args.tracking)}", file=sys.stderr)
    if args.report:
        report = report_markdown(model)
        if args.output:
            print(report, file=sys.stderr)
        else:
            print(report)
            return
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

    _assert_metrics_valid(doc)
    _assert_sorts_valid(doc)

    if args.output:
        with open(args.output, "w", encoding="utf-8") as f:
            f.write(doc)
        print(f"wrote {args.output} ({len(model['benches'])} benchmarks, "
              f"{len(model['commits'])} commits, "
              f"{len(det_rows)} deterministic rows)", file=sys.stderr)
    else:
        sys.stdout.write(doc)


if __name__ == "__main__":
    main()
