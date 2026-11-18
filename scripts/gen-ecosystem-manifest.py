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

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ecosystem_common as eco  # noqa: E402

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXCLUDE_LIST = os.path.join(REPO, "ecosystem", "exclude.txt")
DISTS_DIR = os.path.join(REPO, "ecosystem", "dists")
OUT_PATH = os.path.join(REPO, "site", "content", "ecosystem.json")

# Worst first: the reason a reader opens this page is to find out whether their
# module is in trouble, not to admire the green ones.
STATUS_ORDER = ["red", "partial", "blocked_load", "blocked_dep", "green",
                "no_baseline", "excluded", "skipped"]


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


# The figures the page publishes must equal the ledger summary's.
KPI_KEYS = ("dist_parity", "file_parity", "graded_distributions",
            "green_distributions", "baseline_files", "parity_files")


def check_against_summary(numbers: dict) -> None:
    """Refuse to publish a KPI that differs from the ledger's own summary.

    `ecosystem/summary.json` is the rollup's output for the same ledger
    revision these records came from, so its figures and ours must be equal.
    The comparison is skipped only when the rollup applied a different exclude
    list (an `exclude.txt` edit merged since the last sweep: the page is then
    deliberately one edit ahead of the summary), or the summary predates the
    `excluded_dists` field.
    """
    path = os.path.join(REPO, "ecosystem", "summary.json")
    if not os.path.exists(path):
        return
    with open(path, encoding="utf-8") as fh:
        summary = json.load(fh)
    if summary.get("excluded_dists") != numbers["excluded_dists"]:
        print("note: ecosystem/summary.json was rolled up with another exclude list "
              "(or before it recorded one); not cross-checking the KPI against it")
        return
    diff = {k: (summary.get(k), numbers[k]) for k in KPI_KEYS
            if summary.get(k) != numbers[k]}
    if diff:
        sys.exit("the page KPI disagrees with ecosystem/summary.json (summary, page): "
                 + ", ".join(f"{k}={v}" for k, v in diff.items()))


def main() -> int:
    if not os.path.isdir(DISTS_DIR):
        sys.exit(f"no records at {DISTS_DIR} — run scripts/ecosystem-ledger.sh pull first")

    # A distribution listed in ecosystem/exclude.txt was investigated and judged
    # permanently out of reach by a decision the project will not revisit, so it
    # is shown as `excluded` (with the list's reason) and left out of the KPI,
    # exactly as `no_baseline` is. The record's own status is untouched.
    excluded = eco.load_exclude(EXCLUDE_LIST)

    # eco.load_records refuses two files claiming one distribution: the page
    # is keyed by the record's own `dist`, so a duplicate (what a
    # filename-rule change leaves behind) would list it twice.
    records = eco.load_records(DISTS_DIR)
    rows, measured = [], {}
    for record in records:
        totals = record.get("totals") or {}
        row = {
            "dist": record["dist"],
            "version": record.get("version", "?"),
            "status": record.get("status", "skipped"),
            "axis": record.get("axis", ""),
            "passed": totals.get("parity_files", 0),
            "baseline": totals.get("baseline_files", 0),
            "reason": first_reason(record)[:160],
        }
        if eco.is_excluded(excluded, record["dist"], record.get("version")):
            row.update(status="excluded", passed=0, baseline=0,
                       reason=excluded[record["dist"]][1][:160])
        rows.append(row)
        measured = record.get("measured", measured)

    rows.sort(key=lambda r: (STATUS_ORDER.index(r["status"])
                             if r["status"] in STATUS_ORDER else 99,
                             r["dist"].lower()))

    # The KPI is eco.kpi(), the function the rollup publishes summary.md with,
    # never a recount over the page rows: the two once disagreed on whether an
    # exclude.txt distribution counts, and the page showed 99.2% beside a
    # ledger saying 98.7%.
    numbers = eco.kpi(records, excluded)
    check_against_summary(numbers)

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
        "counts": {"distributions": len(rows),
                   "graded": numbers["graded_distributions"],
                   "green": numbers["green_distributions"], "corpus": corpus_total},
        "coverage": (round(100.0 * len(rows) / corpus_total, 1)
                     if corpus_total else 0.0),
        "dist_parity": numbers["dist_parity"],
        "file_parity": numbers["file_parity"],
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
