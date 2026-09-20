#!/usr/bin/env python3
"""Summarise (and diff) `callgrind_annotate --tree=caller` output.

`callgrind_annotate` splits one function's cost across every *file* its inlined
code came from, so the raw listing shows `lookup_in_package_chain` eight times
at 2.87%, 0.75%, 0.57% ... and never once at its real 5.71%. This merges those
rows, and reports the call counts of each function's callers -- the number that
usually decides whether to make a call cheaper or stop making it.

Usage:
    callgrind_annotate --tree=caller --threshold=99 cg.out > ann.txt
    cg-summary.py ann.txt                      # top self-cost functions
    cg-summary.py ann.txt --callers NAME       # who calls NAME, and how often
    cg-summary.py before.txt after.txt         # A/B a set of functions
    cg-summary.py before.txt after.txt --allocs   # A/B allocation counts

NAME is matched as a substring, so `lookup_in_package` is enough.
"""

import argparse
import collections
import re

ROW = re.compile(r"^\s*([\d,]+) \(\s*[\d.]+%\)\s+([<*=>])\s+(.*)$")


def parse(path):
    """-> (self_cost{fn: Ir}, caller_calls{fn: {caller: calls}}, total)."""
    self_cost = collections.Counter()
    caller_calls = collections.defaultdict(collections.Counter)
    pending = []
    total = 0
    for line in open(path, encoding="utf-8", errors="replace"):
        if "PROGRAM TOTALS" in line:
            m = re.search(r"([\d,]+)", line)
            if m:
                total = int(m.group(1).replace(",", ""))
        m = ROW.match(line)
        if not m:
            continue
        value, kind, rest = int(m.group(1).replace(",", "")), m.group(2), m.group(3)
        if kind == "*":
            fn = clean(rest.split(":", 1)[-1])
            self_cost[fn] += value
            for calls, caller in pending:
                caller_calls[fn][caller] += calls
            pending = []
        elif kind == "<":
            raw = rest.split(":", 1)[-1]
            n = re.search(r"\((\d[\d,]*)x\)", raw)
            pending.append((int(n.group(1).replace(",", "")) if n else 0, clean(raw)))
        else:
            pending = []
    return self_cost, caller_calls, total


def clean(s):
    s = re.sub(r"\s*\(\d[\d,]*x\)", "", s)
    return re.sub(r"\s*\[.*\]$", "", s).strip()


def pct(v, total):
    return f"{100 * v / total:5.2f}%" if total else "    -"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="+")
    ap.add_argument("--callers", metavar="NAME")
    ap.add_argument("--allocs", action="store_true")
    ap.add_argument("-n", type=int, default=20)
    args = ap.parse_args()

    if args.allocs:
        args.callers = args.callers or "__rust_alloc"

    if len(args.files) == 1:
        cost, callers, total = parse(args.files[0])
        if args.callers:
            agg = collections.Counter()
            for fn in callers:
                if args.callers in fn:
                    agg += callers[fn]
            print(f"=== callers of *{args.callers}* ===")
            for name, calls in agg.most_common(args.n):
                print(f"  {calls:>12,}x  {name[:96]}")
            print(f"  {sum(agg.values()):>12,}x  TOTAL")
            return
        print(f"=== top self cost (total {total:,}) ===")
        for fn, v in cost.most_common(args.n):
            print(f"{v:>14,} ({pct(v, total)})  {fn[:92]}")
        return

    before, after = args.files[0], args.files[1]
    bc, bcall, btot = parse(before)
    ac, acall, atot = parse(after)
    if args.callers:
        print(f"=== callers of *{args.callers}*: {before} -> {after} ===")
        agg_b, agg_a = collections.Counter(), collections.Counter()
        for fn in bcall:
            if args.callers in fn:
                agg_b += bcall[fn]
        for fn in acall:
            if args.callers in fn:
                agg_a += acall[fn]
        print(f"  {'':<70}{'before':>12}{'after':>12}")
        for name, _ in (agg_b + agg_a).most_common(args.n):
            print(f"  {name[:68]:<70}{agg_b[name]:>12,}{agg_a[name]:>12,}")
        print(f"  {'TOTAL':<70}{sum(agg_b.values()):>12,}{sum(agg_a.values()):>12,}")
        return

    print(f"{'PROGRAM TOTAL':<58}{btot:>15,}{atot:>15,}"
          f"{(f'{100 * (atot - btot) / btot:+.2f}%' if btot else '-'):>10}")
    moved = sorted(set(bc) | set(ac), key=lambda f: -abs(ac.get(f, 0) - bc.get(f, 0)))
    for fn in moved[: args.n]:
        b, a = bc.get(fn, 0), ac.get(fn, 0)
        d = f"{100 * (a - b) / b:+.1f}%" if b else "new"
        print(f"{fn[:56]:<58}{b:>15,}{a:>15,}{d:>10}")


if __name__ == "__main__":
    main()
