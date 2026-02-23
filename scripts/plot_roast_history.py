#!/usr/bin/env python3

import csv
import html
import math
import sys


def load_rows(history_path: str):
    rows = []
    total_files = None
    with open(history_path, newline="", encoding="utf-8") as file_handle:
        reader = csv.DictReader(file_handle, delimiter="\t")
        for row in reader:
            date = row.get("date", "").strip()
            passed = row.get("pass", "").strip()
            files = row.get("files", "").strip()
            if not date or not passed:
                continue
            try:
                passed_value = int(passed)
            except ValueError:
                continue
            if files:
                try:
                    total_files = int(files)
                except ValueError:
                    pass
            rows.append((date, passed_value))
    return rows, total_files


def nice_step(value_range: int) -> int:
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


def render_svg(rows, output_path: str, total_files: int | None = None):
    width = 1280
    height = 720
    left = 90
    right = 100
    top = 50
    bottom = 120

    chart_width = width - left - right
    chart_height = height - top - bottom

    values = [value for _, value in rows]
    max_value = max(values)

    y_min = 0
    ceiling = max(max_value, total_files or 0)
    pad = max(2, int(ceiling * 0.05) or 2)
    y_max = ceiling + pad
    if y_max == y_min:
        y_max = y_min + 1

    def x_pos(index: int) -> float:
        if len(rows) == 1:
            return left + chart_width / 2
        return left + (chart_width * index / (len(rows) - 1))

    def y_pos(value: int) -> float:
        return top + chart_height * (1 - (value - y_min) / (y_max - y_min))

    y_step = nice_step(y_max - y_min)
    y_tick_start = (y_min // y_step) * y_step
    if y_tick_start < y_min:
        y_tick_start += y_step

    polyline_points = " ".join(
        f"{x_pos(index):.2f},{y_pos(value):.2f}"
        for index, (_, value) in enumerate(rows)
    )

    lines = []
    lines.append('<?xml version="1.0" encoding="UTF-8"?>')
    lines.append(
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">'
    )
    lines.append('<rect width="100%" height="100%" fill="#ffffff"/>')
    lines.append(
        f'<text x="{width / 2:.0f}" y="30" text-anchor="middle" font-size="22" font-family="sans-serif">Roast passed files history</text>'
    )

    lines.append(
        f'<line x1="{left}" y1="{top + chart_height}" x2="{left + chart_width}" y2="{top + chart_height}" stroke="#333" stroke-width="1.5"/>'
    )
    lines.append(
        f'<line x1="{left}" y1="{top}" x2="{left}" y2="{top + chart_height}" stroke="#333" stroke-width="1.5"/>'
    )

    y_tick = y_tick_start
    while y_tick <= y_max:
        y = y_pos(y_tick)
        lines.append(
            f'<line x1="{left}" y1="{y:.2f}" x2="{left + chart_width}" y2="{y:.2f}" stroke="#e5e7eb" stroke-width="1"/>'
        )
        lines.append(
            f'<text x="{left - 12}" y="{y + 5:.2f}" text-anchor="end" font-size="12" fill="#111" font-family="sans-serif">{y_tick}</text>'
        )
        y_tick += y_step

    x_label_step = max(1, len(rows) // 8)
    for index, (date, _) in enumerate(rows):
        if index % x_label_step != 0 and index != len(rows) - 1:
            continue
        x = x_pos(index)
        lines.append(
            f'<line x1="{x:.2f}" y1="{top + chart_height}" x2="{x:.2f}" y2="{top + chart_height + 6}" stroke="#333" stroke-width="1"/>'
        )
        # Show date part only on x-axis label (strip time after 'T')
        display_date = date.split("T")[0] if "T" in date else date
        escaped_date = html.escape(display_date)
        lines.append(
            f'<text x="{x:.2f}" y="{top + chart_height + 24}" text-anchor="end" font-size="11" fill="#111" transform="rotate(-40 {x:.2f},{top + chart_height + 24})" font-family="sans-serif">{escaped_date}</text>'
        )

    if total_files is not None:
        goal_y = y_pos(total_files)
        lines.append(
            f'<line x1="{left}" y1="{goal_y:.2f}" x2="{left + chart_width}" y2="{goal_y:.2f}" stroke="#dc2626" stroke-width="1.5" stroke-dasharray="8,4"/>'
        )
        lines.append(
            f'<text x="{left + chart_width + 4}" y="{goal_y + 4:.2f}" font-size="12" fill="#dc2626" font-family="sans-serif">Goal: {total_files}</text>'
        )

    lines.append(
        f'<polyline fill="none" stroke="#2563eb" stroke-width="3" points="{polyline_points}"/>'
    )

    for index, (_, value) in enumerate(rows):
        x = x_pos(index)
        y = y_pos(value)
        lines.append(
            f'<circle cx="{x:.2f}" cy="{y:.2f}" r="3.2" fill="#1d4ed8"/>'
        )

    lines.append(
        f'<text x="{left + chart_width / 2:.0f}" y="{height - 25}" text-anchor="middle" font-size="14" fill="#111" font-family="sans-serif">Date</text>'
    )
    lines.append(
        f'<text x="24" y="{top + chart_height / 2:.0f}" text-anchor="middle" font-size="14" fill="#111" transform="rotate(-90 24,{top + chart_height / 2:.0f})" font-family="sans-serif">Passed files</text>'
    )
    lines.append("</svg>")

    with open(output_path, "w", encoding="utf-8") as file_handle:
        file_handle.write("\n".join(lines) + "\n")


def main():
    if len(sys.argv) != 3:
        print("usage: plot_roast_history.py <HISTORY.tsv> <output.svg>", file=sys.stderr)
        return 1

    history_path = sys.argv[1]
    output_path = sys.argv[2]

    rows, total_files = load_rows(history_path)
    if not rows:
        print("No valid history rows found", file=sys.stderr)
        return 1

    render_svg(rows, output_path, total_files)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
