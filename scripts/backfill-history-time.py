#!/usr/bin/env python3
"""Backfill HISTORY.tsv date column with datetime from git log.

Replaces date-only entries (e.g. '2026-02-12') with datetime entries
(e.g. '2026-02-12T15:30') by looking up each commit's author date via git log.
"""

import csv
import subprocess
import sys


def get_commit_dates() -> dict[str, str]:
    """Return a mapping of short-hash -> 'YYYY-MM-DDTHH:MM' from git log."""
    result = subprocess.run(
        ["git", "log", "--format=%h %aI", "--all"],
        capture_output=True,
        text=True,
        check=True,
    )
    mapping: dict[str, str] = {}
    for line in result.stdout.strip().splitlines():
        parts = line.split(" ", 1)
        if len(parts) != 2:
            continue
        short_hash, iso_date = parts
        # iso_date is like '2026-02-23T21:18:15+09:00'
        # Truncate to 'YYYY-MM-DDTHH:MM'
        datetime_short = iso_date[:16]
        mapping[short_hash] = datetime_short
    return mapping


def main():
    history_path = sys.argv[1] if len(sys.argv) > 1 else "HISTORY.tsv"

    commit_dates = get_commit_dates()

    with open(history_path, newline="", encoding="utf-8") as f:
        lines = f.readlines()

    if not lines:
        print("Empty HISTORY.tsv", file=sys.stderr)
        return 1

    header = lines[0]
    new_lines = [header]
    updated = 0

    for line in lines[1:]:
        if not line.strip():
            new_lines.append(line)
            continue
        fields = line.rstrip("\n").split("\t")
        if len(fields) < 2:
            new_lines.append(line)
            continue

        date_field = fields[0]
        commit_field = fields[1]

        # Only backfill if date is date-only (no 'T' present)
        if "T" not in date_field and commit_field in commit_dates:
            fields[0] = commit_dates[commit_field]
            updated += 1

        new_lines.append("\t".join(fields) + "\n")

    with open(history_path, "w", encoding="utf-8") as f:
        f.writelines(new_lines)

    print(f"Updated {updated} entries in {history_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
