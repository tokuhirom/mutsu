#!/usr/bin/env python3
"""Reconstruct the todo/tickets and todo/deep file-count history from git log.

Walks every commit that touched todo/tickets/*.md or todo/deep/*.md (in
chronological order), replays the add/delete/rename events, and reports the
file count in each directory right after that commit. Useful for tracking
whether the backlog is growing or shrinking over time.

Usage:
    scripts/todo-backlog-history.py [--tsv PATH] [--svg PATH]

With no arguments, prints a TSV (date, commit, tickets_count, deep_count) to
stdout. Pass --svg to also render a two-series line chart.
"""

import argparse
import html
import math
import subprocess
import sys

TICKETS_DIR = "todo/tickets/"
DEEP_DIR = "todo/deep/"


def which_set(path, tickets, deep):
    if path.startswith(TICKETS_DIR) and path.endswith(".md"):
        return tickets
    if path.startswith(DEEP_DIR) and path.endswith(".md"):
        return deep
    return None


def collect_history():
    proc = subprocess.run(
        [
            "git",
            "log",
            "--reverse",
            "--format=COMMIT\t%H\t%cI",
            "--name-status",
            "--",
            "todo/tickets",
            "todo/deep",
        ],
        capture_output=True,
        text=True,
        check=True,
    )

    tickets = set()
    deep = set()
    rows = []

    commit_hash = None
    commit_date = None
    changed = False

    def flush():
        if commit_hash is not None and changed:
            rows.append((commit_date, commit_hash[:8], len(tickets), len(deep)))

    for line in proc.stdout.splitlines():
        if line.startswith("COMMIT\t"):
            flush()
            _, commit_hash, commit_date = line.split("\t")
            changed = False
            continue
        if not line.strip():
            continue

        fields = line.split("\t")
        status = fields[0]

        if status.startswith("R") or status.startswith("C"):
            old_path, new_path = fields[1], fields[2]
            old_set = which_set(old_path, tickets, deep)
            new_set = which_set(new_path, tickets, deep)
            if old_set is not None and old_path in old_set:
                old_set.discard(old_path)
                changed = True
            if new_set is not None:
                new_set.add(new_path)
                changed = True
        elif status == "A":
            target = which_set(fields[1], tickets, deep)
            if target is not None:
                target.add(fields[1])
                changed = True
        elif status == "D":
            target = which_set(fields[1], tickets, deep)
            if target is not None and fields[1] in target:
                target.discard(fields[1])
                changed = True
        # "M" (modify) never changes the file count.

    flush()
    return rows


def write_tsv(rows, out):
    out.write("date\tcommit\ttickets_count\tdeep_count\n")
    for date, commit, tickets_count, deep_count in rows:
        out.write(f"{date}\t{commit}\t{tickets_count}\t{deep_count}\n")


def nice_step(value_range):
    if value_range <= 0:
        return 1
    rough = max(1, math.ceil(value_range / 5))
    magnitude = 10 ** int(math.floor(math.log10(rough)))
    ratio = rough / magnitude
    if ratio <= 1:
        return magnitude
    if ratio <= 2:
        return 2 * magnitude
    if ratio <= 5:
        return 5 * magnitude
    return 10 * magnitude


def render_svg(rows, output_path):
    width, height = 1280, 720
    left, right, top, bottom = 90, 140, 50, 120
    chart_width = width - left - right
    chart_height = height - top - bottom

    tickets_values = [row[2] for row in rows]
    deep_values = [row[3] for row in rows]
    y_max = max(tickets_values + deep_values)
    y_max += max(2, int(y_max * 0.08) or 2)

    def x_pos(index):
        if len(rows) == 1:
            return left + chart_width / 2
        return left + (chart_width * index / (len(rows) - 1))

    def y_pos(value):
        return top + chart_height * (1 - value / y_max)

    y_step = nice_step(y_max)

    lines = []
    lines.append('<?xml version="1.0" encoding="UTF-8"?>')
    lines.append(
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">'
    )
    lines.append('<rect width="100%" height="100%" fill="#ffffff"/>')
    lines.append(
        f'<text x="{width / 2:.0f}" y="30" text-anchor="middle" font-size="22" '
        f'font-family="sans-serif">todo/tickets and todo/deep file-count history</text>'
    )

    lines.append(
        f'<line x1="{left}" y1="{top + chart_height}" x2="{left + chart_width}" '
        f'y2="{top + chart_height}" stroke="#333" stroke-width="1.5"/>'
    )
    lines.append(
        f'<line x1="{left}" y1="{top}" x2="{left}" y2="{top + chart_height}" '
        f'stroke="#333" stroke-width="1.5"/>'
    )

    y_tick = 0
    while y_tick <= y_max:
        y = y_pos(y_tick)
        lines.append(
            f'<line x1="{left}" y1="{y:.2f}" x2="{left + chart_width}" y2="{y:.2f}" '
            f'stroke="#e5e7eb" stroke-width="1"/>'
        )
        lines.append(
            f'<text x="{left - 12}" y="{y + 5:.2f}" text-anchor="end" font-size="12" '
            f'fill="#111" font-family="sans-serif">{y_tick}</text>'
        )
        y_tick += y_step

    x_label_step = max(1, len(rows) // 8)
    for index, row in enumerate(rows):
        if index % x_label_step != 0 and index != len(rows) - 1:
            continue
        x = x_pos(index)
        lines.append(
            f'<line x1="{x:.2f}" y1="{top + chart_height}" x2="{x:.2f}" '
            f'y2="{top + chart_height + 6}" stroke="#333" stroke-width="1"/>'
        )
        display_date = row[0].split("T")[0]
        escaped_date = html.escape(display_date)
        lines.append(
            f'<text x="{x:.2f}" y="{top + chart_height + 24}" text-anchor="end" '
            f'font-size="11" fill="#111" transform="rotate(-40 {x:.2f},'
            f'{top + chart_height + 24})" font-family="sans-serif">{escaped_date}</text>'
        )

    def series(values, color):
        points = " ".join(f"{x_pos(i):.2f},{y_pos(v):.2f}" for i, v in enumerate(values))
        lines.append(f'<polyline fill="none" stroke="{color}" stroke-width="2.5" points="{points}"/>')
        for i, v in enumerate(values):
            lines.append(f'<circle cx="{x_pos(i):.2f}" cy="{y_pos(v):.2f}" r="2.6" fill="{color}"/>')

    series(tickets_values, "#2a78d6")
    series(deep_values, "#eb6834")

    legend_x = left + chart_width + 16
    lines.append(f'<rect x="{legend_x}" y="{top}" width="12" height="12" fill="#2a78d6"/>')
    lines.append(
        f'<text x="{legend_x + 18}" y="{top + 10}" font-size="13" fill="#111" '
        f'font-family="sans-serif">tickets ({tickets_values[-1]})</text>'
    )
    lines.append(f'<rect x="{legend_x}" y="{top + 22}" width="12" height="12" fill="#eb6834"/>')
    lines.append(
        f'<text x="{legend_x + 18}" y="{top + 32}" font-size="13" fill="#111" '
        f'font-family="sans-serif">deep ({deep_values[-1]})</text>'
    )

    lines.append(
        f'<text x="{left + chart_width / 2:.0f}" y="{height - 25}" text-anchor="middle" '
        f'font-size="14" fill="#111" font-family="sans-serif">Commit date</text>'
    )
    lines.append(
        f'<text x="24" y="{top + chart_height / 2:.0f}" text-anchor="middle" font-size="14" '
        f'fill="#111" transform="rotate(-90 24,{top + chart_height / 2:.0f})" '
        f'font-family="sans-serif">File count</text>'
    )
    lines.append("</svg>")

    with open(output_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tsv", help="write the TSV history to this path (default: stdout)")
    parser.add_argument("--svg", help="also render a line chart to this path")
    args = parser.parse_args()

    rows = collect_history()
    if not rows:
        print("No history found for todo/tickets or todo/deep", file=sys.stderr)
        return 1

    if args.tsv:
        with open(args.tsv, "w", encoding="utf-8") as f:
            write_tsv(rows, f)
    else:
        write_tsv(rows, sys.stdout)

    if args.svg:
        render_svg(rows, args.svg)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
