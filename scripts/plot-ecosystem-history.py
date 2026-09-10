#!/usr/bin/env python3
"""Render the ecosystem-parity KPI time series as a standalone SVG.

Input is `ecosystem/history.tsv` (one row per sweep; see docs/ecosystem-parity.md
section 4); output is an SVG that renders in a README, in the GitHub repo view, and
on the site page, with no JavaScript and no external assets.

    scripts/plot-ecosystem-history.py ecosystem/history.tsv ecosystem/history.svg

Why this is not `scripts/plot_roast_history.py`:

* **The x axis is time, not sample index.** `HISTORY.tsv` gets a row on every
  main push, so evenly-spaced samples are a fair picture. Ecosystem sweeps are
  operator-run and irregular (ADR-0085 D9), so index spacing would show a
  six-week gap and a same-day re-run as the same distance.
* **The denominator moves.** A rakudo upgrade re-bases every baseline, so the
  series is not comparable across it. Those points are drawn as labelled
  vertical rules rather than left to look like progress or regression.
* **Three series, all rates.** `dist_parity` is the published headline;
  `file_parity` and `assertion_parity` are what the work is steered by
  (ADR-0085 D3).

The SVG carries a `prefers-color-scheme` stylesheet, so it stays legible in a
dark README. Browsers that ignore it get the light palette.
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import html
import sys

WIDTH = 1280
HEIGHT = 640
LEFT = 78
RIGHT = 210
TOP = 74
BOTTOM = 92

SERIES = [
    ("dist_parity", "dist parity (published)", "series-a", 3.2),
    ("file_parity", "file parity (steers the work)", "series-b", 2.0),
    ("assertion_parity", "assertion parity", "series-c", 2.0),
]

STYLE = """
  .bg { fill: #ffffff; }
  .title { fill: #111827; font: 600 21px sans-serif; }
  .subtitle { fill: #6b7280; font: 400 13px sans-serif; }
  .axis { stroke: #374151; stroke-width: 1.4; }
  .grid { stroke: #e5e7eb; stroke-width: 1; }
  .tick { fill: #374151; font: 400 12px sans-serif; }
  .legend { fill: #111827; font: 400 13px sans-serif; }
  .value { font: 600 13px sans-serif; }
  .rebase { stroke: #9ca3af; stroke-width: 1.2; stroke-dasharray: 5 4; }
  .rebase-label { fill: #6b7280; font: 400 11px sans-serif; }
  .series-a { stroke: #b45309; fill: #b45309; }
  .series-b { stroke: #1d4ed8; fill: #1d4ed8; }
  .series-c { stroke: #047857; fill: #047857; }
  @media (prefers-color-scheme: dark) {
    .bg { fill: #0d1117; }
    .title { fill: #e6edf3; }
    .subtitle { fill: #9198a1; }
    .axis { stroke: #6e7681; }
    .grid { stroke: #21262d; }
    .tick { fill: #9198a1; }
    .legend { fill: #e6edf3; }
    .rebase { stroke: #6e7681; }
    .rebase-label { fill: #9198a1; }
    .series-a { stroke: #f0b429; fill: #f0b429; }
    .series-b { stroke: #6ea8fe; fill: #6ea8fe; }
    .series-c { stroke: #4ade80; fill: #4ade80; }
  }
"""


def load_rows(path: str) -> list[dict]:
    """Rows with a parseable date, oldest first. A rate may be blank or absent;
    that series is simply not drawn for that row."""
    rows = []
    with open(path, newline="", encoding="utf-8") as fh:
        for raw in csv.DictReader(fh, delimiter="\t"):
            date = (raw.get("date") or "").strip()
            try:
                day = dt.date.fromisoformat(date)
            except ValueError:
                continue
            row = {"date": day, "raku": (raw.get("raku_version") or "").strip()}
            for key, _label, _cls, _w in SERIES:
                row[key] = to_rate(raw.get(key))
            for key in ("dists", "measured", "blocked_dep", "baseline_files"):
                row[key] = (raw.get(key) or "").strip()
            rows.append(row)
    rows.sort(key=lambda r: r["date"])
    return rows


def to_rate(text: str | None) -> float | None:
    """Parse a rate column, which `history.tsv` stores as a **percentage**
    (`72.0`, optionally with a trailing `%`).

    Deliberately not lenient about fractions: a "0.83 means 83%" rule cannot be
    told apart from a genuine 0.83% -- which is exactly the reading a campaign
    starting from near zero would produce -- so the chart would silently draw a
    near-empty corpus as near-complete. One convention, enforced.
    """
    if text is None:
        return None
    s = text.strip().rstrip("%")
    if not s:
        return None
    try:
        value = float(s)
    except ValueError:
        return None
    if not 0.0 <= value <= 100.0:
        raise SystemExit(f"rate out of range: {text!r} (history.tsv stores percentages, 0-100)")
    return value


def render(rows: list[dict], out_path: str, title: str) -> None:
    chart_w = WIDTH - LEFT - RIGHT
    chart_h = HEIGHT - TOP - BOTTOM

    first, last = rows[0]["date"], rows[-1]["date"]
    span = max((last - first).days, 1)

    def x_of(day: dt.date) -> float:
        if len(rows) == 1:
            return LEFT + chart_w / 2
        return LEFT + chart_w * (day - first).days / span

    def y_of(pct: float) -> float:
        return TOP + chart_h * (1 - pct / 100.0)

    out: list[str] = []
    add = out.append
    add('<?xml version="1.0" encoding="UTF-8"?>')
    add(f'<svg xmlns="http://www.w3.org/2000/svg" width="{WIDTH}" height="{HEIGHT}" '
        f'viewBox="0 0 {WIDTH} {HEIGHT}" role="img" aria-label="{html.escape(title)}">')
    add(f"<style>{STYLE}</style>")
    add(f'<rect class="bg" width="100%" height="100%"/>')
    add(f'<text class="title" x="{LEFT}" y="34">{html.escape(title)}</text>')
    add(f'<text class="subtitle" x="{LEFT}" y="55">{html.escape(subtitle(rows))}</text>')

    for pct in range(0, 101, 10):
        y = y_of(pct)
        add(f'<line class="grid" x1="{LEFT}" y1="{y:.1f}" x2="{LEFT + chart_w}" y2="{y:.1f}"/>')
        add(f'<text class="tick" x="{LEFT - 10}" y="{y + 4:.1f}" text-anchor="end">{pct}%</text>')
    add(f'<line class="axis" x1="{LEFT}" y1="{TOP + chart_h}" x2="{LEFT + chart_w}" y2="{TOP + chart_h}"/>')
    add(f'<line class="axis" x1="{LEFT}" y1="{TOP}" x2="{LEFT}" y2="{TOP + chart_h}"/>')

    # A rakudo change re-bases every baseline: the series is not comparable
    # across it, so say so on the chart instead of letting the step read as news.
    for prev, row in zip(rows, rows[1:]):
        if row["raku"] and prev["raku"] and row["raku"] != prev["raku"]:
            x = x_of(row["date"])
            add(f'<line class="rebase" x1="{x:.1f}" y1="{TOP}" x2="{x:.1f}" y2="{TOP + chart_h}"/>')
            add(f'<text class="rebase-label" x="{x + 5:.1f}" y="{TOP + 13}">'
                f'rakudo {html.escape(row["raku"])} &#8212; baseline re-based</text>')

    for date_label_x, label in date_labels(rows, x_of):
        add(f'<text class="tick" x="{date_label_x:.1f}" y="{TOP + chart_h + 22}" '
            f'text-anchor="middle">{html.escape(label)}</text>')

    for index, (key, label, cls, stroke) in enumerate(SERIES):
        points = [(x_of(r["date"]), y_of(r[key])) for r in rows if r[key] is not None]
        if not points:
            continue
        path = " ".join(f"{x:.1f},{y:.1f}" for x, y in points)
        add(f'<polyline class="{cls}" points="{path}" fill="none" stroke-width="{stroke}" '
            f'stroke-linejoin="round" stroke-linecap="round"/>')
        for x, y in points:
            add(f'<circle class="{cls}" cx="{x:.1f}" cy="{y:.1f}" r="{3.4 if stroke > 3 else 2.6}" stroke="none"/>')
        # The current value, spelled out: with a handful of sweeps the reader
        # wants the number, not an eyeballed position on the axis.
        vx, vy = points[-1]
        value = next(r[key] for r in reversed(rows) if r[key] is not None)
        add(f'<text class="value {cls}" x="{vx + 9:.1f}" y="{vy + 4:.1f}" stroke="none">{value:.1f}%</text>')
        ly = TOP + 6 + index * 21
        add(f'<line class="{cls}" x1="{LEFT + chart_w + 24}" y1="{ly:.1f}" '
            f'x2="{LEFT + chart_w + 48}" y2="{ly:.1f}" stroke-width="{stroke}"/>')
        add(f'<text class="legend" x="{LEFT + chart_w + 54}" y="{ly + 4:.1f}">{html.escape(label)}</text>')

    add("</svg>")
    with open(out_path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(out) + "\n")


def subtitle(rows: list[dict]) -> str:
    last = rows[-1]
    bits = [f"latest sweep {last['date'].isoformat()}"]
    if last["measured"] and last["dists"]:
        bits.append(f"{last['measured']} of {last['dists']} distributions measured")
    if last["blocked_dep"]:
        bits.append(f"{last['blocked_dep']} blocked on dependencies")
    if last["raku"]:
        bits.append(f"baseline rakudo {last['raku']}")
    return "  ·  ".join(bits)


def date_labels(rows: list[dict], x_of) -> list[tuple[float, str]]:
    """At most 8 labels, always including the first and last sweep."""
    if len(rows) <= 8:
        picks = list(range(len(rows)))
    else:
        step = (len(rows) - 1) / 7
        picks = sorted({round(i * step) for i in range(8)} | {0, len(rows) - 1})
    return [(x_of(rows[i]["date"]), rows[i]["date"].isoformat()) for i in picks]


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("history", help="ecosystem/history.tsv")
    ap.add_argument("output", help="SVG to write")
    ap.add_argument("--title", default="mutsu ecosystem parity against rakudo")
    args = ap.parse_args()

    rows = load_rows(args.history)
    if not rows:
        print(f"no usable rows in {args.history}", file=sys.stderr)
        return 1
    render(rows, args.output, args.title)
    print(f"{args.output}: {len(rows)} sweep(s), "
          f"{rows[0]['date'].isoformat()} .. {rows[-1]['date'].isoformat()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
