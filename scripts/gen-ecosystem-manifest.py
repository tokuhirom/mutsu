#!/usr/bin/env python3
"""Generate site/content/ecosystem.json from the parity ledger.

The public "does my module work on mutsu?" page (site/ecosystem.html) is built
from `ecosystem/dists/**.json`, so it can never drift from the measurements: the
records are the authority, this file is a projection of them.

    python3 scripts/gen-ecosystem-manifest.py

Run after a sweep, and again at deploy time in pages.yml, so the committed
snapshot and the deployed page stay in step -- the same arrangement
`scripts/gen-batteries-manifest.py` uses for the batteries page.

The projection is lossy on purpose. A record carries every test file with both
sides' TAP counts; the page needs one row per distribution plus enough of a
reason to be worth reading. Anyone who wants the detail reads the record.
"""

from __future__ import annotations

import json
import os
import sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DISTS_DIR = os.path.join(REPO, "ecosystem", "dists")
OUT_PATH = os.path.join(REPO, "site", "content", "ecosystem.json")

# Worst first: the reason a reader opens this page is to find out whether their
# module is in trouble, not to admire the green ones.
STATUS_ORDER = ["red", "partial", "blocked_load", "blocked_dep", "green",
                "no_baseline", "skipped"]


def first_reason(record: dict) -> str:
    """One line saying why this distribution is not green, or ''."""
    status = record.get("status")
    if status == "blocked_dep":
        missing = record.get("deps", {}).get("unresolved") or []
        return "missing dependency: " + ", ".join(missing[:3]) if missing else ""
    if status == "blocked_load":
        for module, detail in (record.get("load") or {}).items():
            if detail not in ("ok", "raku_also_fails"):
                return f"{module}: {detail}"
        return ""
    for entry in record.get("files") or []:
        if entry.get("cmp") in ("regression", "partial"):
            return (entry.get("mutsu") or {}).get("first_failure") or ""
    return ""


def main() -> int:
    if not os.path.isdir(DISTS_DIR):
        sys.exit(f"no records at {DISTS_DIR} — run scripts/ecosystem-sweep.py first")

    rows, measured = [], {}
    # The page is keyed by the record's own `dist`, never by its filename, so a
    # second file claiming the same distribution would list it twice with two
    # different verdicts. That is what a filename-rule change leaves behind if
    # the existing records are not renamed with it, so refuse it here as well as
    # in the rollup.
    where = {}
    for dirpath, _dirs, names in os.walk(DISTS_DIR):
        for name in names:
            if not name.endswith(".json"):
                continue
            path = os.path.join(dirpath, name)
            with open(path, encoding="utf-8") as fh:
                record = json.load(fh)
            if record.get("dist") in where:
                sys.exit(f"two records claim the distribution "
                         f"{record['dist']!r}:\n  {where[record['dist']]}\n  {path}")
            where[record.get("dist")] = path
            totals = record.get("totals") or {}
            rows.append({
                "dist": record["dist"],
                "version": record.get("version", "?"),
                "status": record.get("status", "skipped"),
                "axis": record.get("axis", ""),
                "passed": totals.get("parity_files", 0),
                "baseline": totals.get("baseline_files", 0),
                "reason": first_reason(record)[:160],
            })
            measured = record.get("measured", measured)

    rows.sort(key=lambda r: (STATUS_ORDER.index(r["status"])
                             if r["status"] in STATUS_ORDER else 99,
                             r["dist"].lower()))

    graded = [r for r in rows if r["baseline"] > 0]
    green = [r for r in graded if r["status"] == "green"]
    baseline_files = sum(r["baseline"] for r in rows)
    passed_files = sum(r["passed"] for r in rows)

    # Whether the KPI chart exists is decided here, not by the page probing for
    # it: a fetch that 404s on every load is a console error on a production
    # page, and the generator already knows the answer.
    has_chart = os.path.exists(os.path.join(REPO, "ecosystem", "history.svg"))

    # How much of the corpus these numbers speak for. A sweep runs shard by
    # shard and the site is redeployed from whatever has landed, so without this
    # a third of the corpus would publish a parity figure that reads as the
    # whole ecosystem's. Read from the index snapshot the sweep wrote, so it
    # cannot drift from what was actually swept.
    corpus_total = 0
    snapshot_path = os.path.join(REPO, "ecosystem", "index-snapshot.json")
    if os.path.exists(snapshot_path):
        with open(snapshot_path, encoding="utf-8") as fh:
            corpus_total = (json.load(fh).get("fez") or {}).get("dists", 0)

    manifest = {
        "generated_from": "ecosystem/dists",
        "has_chart": has_chart,
        "measured": {k: measured.get(k) for k in
                     ("date", "mutsu_commit", "mutsu_version", "raku_version", "host")},
        "counts": {"distributions": len(rows), "graded": len(graded),
                   "green": len(green), "corpus": corpus_total},
        "coverage": (round(100.0 * len(rows) / corpus_total, 1)
                     if corpus_total else 0.0),
        "dist_parity": round(100.0 * len(green) / len(graded), 1) if graded else 0.0,
        "file_parity": (round(100.0 * passed_files / baseline_files, 1)
                        if baseline_files else 0.0),
        "distributions": rows,
    }
    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    with open(OUT_PATH, "w", encoding="utf-8") as fh:
        json.dump(manifest, fh, indent=1, sort_keys=True, ensure_ascii=False)
        fh.write("\n")
    print(f"{OUT_PATH}: {len(rows)} distributions, "
          f"dist parity {manifest['dist_parity']}%")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
