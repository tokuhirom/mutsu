#!/usr/bin/env python3
"""pick-dist.py — draw a random actionable distribution out of the ecosystem ledger.

    .agents/skills/ecosystem-dist-roulette/pick-dist.py

Prints a shortlist (default 5) of distributions sampled *uniformly* from
`ecosystem/dists/**.json`, so the agent can take the first one the lock board
does not already hold without re-rolling and without re-reading 251 records.

Uniform is the point. Picking the cheapest-looking record games the parity KPI
exactly the way cherry-picking easy roast tests games the roast count, and the
number the ledger publishes is only meaningful if the sample behind it is
unbiased. The one legitimate reason to skip a candidate is that somebody else
holds it (the lock board, or an open PR) — never that it looks hard.

Default pool: `status` in red / partial / blocked_load, `axis` == pure. Those
are the records where fixing the interpreter is what moves them; `guts` and
`native` are usually the issue-filing case (see the ecosystem-dist-fix skill),
`green` needs nothing, and `no_baseline` / `blocked_dep` are not charged to
mutsu at all.
"""

from __future__ import annotations

import argparse
import glob
import json
import os
import random
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))
DISTS = os.path.join(REPO, "ecosystem", "dists")

ACTIONABLE = ("red", "partial", "blocked_load")
ALL_STATUS = ACTIONABLE + ("green", "no_baseline", "blocked_dep", "skipped")


def load_records() -> list[dict]:
    out = []
    for path in sorted(glob.glob(os.path.join(DISTS, "*", "*.json"))):
        try:
            with open(path, encoding="utf-8") as fh:
                rec = json.load(fh)
        except (OSError, ValueError) as exc:
            print(f"warning: skipping {path}: {exc}", file=sys.stderr)
            continue
        rec["_path"] = os.path.relpath(path, REPO)
        out.append(rec)
    return out


def actionable_files(rec: dict) -> int:
    return sum(1 for f in rec.get("files", [])
               if f.get("cmp") in ("regression", "partial"))


def summarize(rec: dict) -> dict:
    totals = rec.get("totals", {}) or {}
    load = rec.get("load", {}) or {}
    bad_load = {m: v for m, v in load.items() if v != "ok"}
    return {
        "dist": rec.get("dist"),
        "version": rec.get("version"),
        "status": rec.get("status"),
        "axis": rec.get("axis"),
        "record": rec["_path"],
        "baseline_files": totals.get("baseline_files", 0),
        "parity_files": totals.get("parity_files", 0),
        "actionable_files": actionable_files(rec),
        "load_errors": bad_load,
        "measured": (rec.get("measured", {}) or {}).get("date"),
        "mutsu_commit": (rec.get("measured", {}) or {}).get("mutsu_commit"),
        "deps": len((rec.get("deps", {}) or {}).get("resolved", []) or []),
    }


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--count", type=int, default=5,
                    help="how many candidates to draw (default 5)")
    ap.add_argument("--status", action="append", choices=ALL_STATUS,
                    help=f"status to draw from; repeatable (default: {' '.join(ACTIONABLE)})")
    ap.add_argument("--axis", default="pure", choices=("pure", "guts", "native", "any"),
                    help="axis to draw from (default pure)")
    ap.add_argument("--exclude", action="append", default=[], metavar="DIST",
                    help="distribution to leave out; repeatable (use for held locks)")
    ap.add_argument("--exclude-file", metavar="PATH",
                    help="file with one distribution name per line to leave out")
    ap.add_argument("--seed", help="seed the draw, to make it reproducible")
    ap.add_argument("--json", action="store_true", help="emit the shortlist as JSON")
    ap.add_argument("--pool", action="store_true",
                    help="print the whole matching pool instead of a sample")
    args = ap.parse_args()

    statuses = tuple(args.status) if args.status else ACTIONABLE
    excluded = {name.strip() for name in args.exclude if name.strip()}
    if args.exclude_file:
        with open(args.exclude_file, encoding="utf-8") as fh:
            excluded |= {line.strip() for line in fh if line.strip()
                         and not line.startswith("#")}

    records = load_records()
    if not records:
        print(f"error: no records under {DISTS}", file=sys.stderr)
        return 2

    pool = [r for r in records
            if r.get("status") in statuses
            and (args.axis == "any" or r.get("axis") == args.axis)
            and r.get("dist") not in excluded]

    if not pool:
        print("error: nothing matches that filter (all excluded?)", file=sys.stderr)
        return 1

    rng = random.Random(args.seed) if args.seed is not None else random.SystemRandom()
    picked = sorted(pool, key=lambda r: r.get("dist") or "") if args.pool \
        else rng.sample(pool, min(args.count, len(pool)))
    rows = [summarize(r) for r in picked]

    if args.json:
        json.dump({"pool": len(pool), "excluded": sorted(excluded),
                   "statuses": list(statuses), "axis": args.axis,
                   "candidates": rows}, sys.stdout, indent=2, sort_keys=True)
        print()
        return 0

    print(f"# pool: {len(pool)} records "
          f"(status {'/'.join(statuses)}, axis {args.axis}"
          f"{', %d excluded' % len(excluded) if excluded else ''})")
    for i, row in enumerate(rows, 1):
        head = (f"{i}. {row['dist']} {row['version']}  "
                f"[{row['status']} / {row['axis']}]")
        print(head)
        if row["status"] == "blocked_load":
            for mod, err in list(row["load_errors"].items())[:3]:
                print(f"     load {mod}: {err}")
        else:
            print(f"     files: {row['parity_files']}/{row['baseline_files']} at parity, "
                  f"{row['actionable_files']} actionable")
        print(f"     deps: {row['deps']}   measured {row['measured']} "
              f"@ {row['mutsu_commit']}   {row['record']}")
    print()
    print("# take the FIRST candidate the lock board does not hold. Skipping one "
          "because it looks hard is cherry-picking.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
