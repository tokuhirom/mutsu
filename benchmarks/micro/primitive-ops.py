#!/usr/bin/env python3
"""Per-primitive-operation cost, mutsu vs rakudo.

    cargo build --release && python3 benchmarks/micro/primitive-ops.py

# Why this exists

A whole-program ratio ("JSON::Fast is 59x rakudo") says nothing about where
the 59 comes from, and a flat profile answers "which function is hot", which
is a different question again -- a function can be hot because it is called
too often, not because it is slow. This asks the third question: for one
Raku operation, how much does mutsu cost against rakudo?

It is what showed that mutsu's gap is NOT a uniform per-operation tax.
Arithmetic, string and container operations run at 4-10x; a method call runs
at ~300x. See news/2026-09/the-gap-is-the-call-not-the-operation.md.

# Method

Each operation is timed inside the SAME loop skeleton at N and at 2N
iterations; `(t(2N) - t(N)) / N` is its per-iteration cost with process
startup, module load and compile time cancelled *exactly* rather than
estimated. Subtracting the empty loop's own per-iteration cost then leaves
the operation alone.

N is chosen per engine and per operation -- doubled until the 2N-N difference
is large enough to measure -- so the two engines' raw N differ. The reported
number is per iteration, so that is not a comparison error; it is what makes
one table cover operations whose costs span three orders of magnitude.

Three runs per point, minimum taken: a minimum is the run least disturbed by
the rest of the machine, where a mean folds that disturbance in.

# Reading the output

The ratio column is `(mutsu - mutsu_empty) / (raku - raku_empty)`. A row whose
rakudo cost is under half a nanosecond prints `-` rather than a ratio built on
noise. The `empty (loop)` row is the loop skeleton itself, which every other
row has had subtracted, and is worth reading on its own: it is the cost of
`while`, a comparison and an increment.
"""

import os
import subprocess
import sys
import time

# Generated scripts are throwaway; `tmp/` is the project-local gitignored
# scratch directory AGENTS.md reserves for exactly this.
SCRATCH = "tmp/micro"

PREAMBLE = {
    "array_read": "my @a = 1..10;",
    "array_store": "my @a = 1..10;",
    "array_push_pop": "my @a = 1..10;",
    "hash_read": "my %h = k => 1, j => 2;",
    "hash_store": "my %h = k => 1, j => 2;",
    "sub_call": "sub f() { 1 }",
    "method_call": "class C { has $.v; method m() { 1 } }\nmy $o = C.new(v => 1);",
    "attr_read": "class C { has $.v; method m() { 1 } }\nmy $o = C.new(v => 1);",
    "new": "class C { has $.v; method m() { 1 } }",
    "str_concat": 'my $a = "abc"; my $b = "def";',
}

BODY = {
    "empty": "",
    "int_add": "$t = $t + 1;",
    "int_cmp": "$t = 1 < 2 ?? 1 !! 0;",
    "str_concat": "$t = $a ~ $b;",
    "array_read": "$t = @a[3];",
    "array_store": "@a[3] = 1;",
    "array_push_pop": "@a.push(1); @a.pop;",
    "hash_read": "$t = %h<k>;",
    "hash_store": "%h<k> = 1;",
    "sub_call": "$t = f();",
    "method_call": "$t = $o.m();",
    "attr_read": "$t = $o.v;",
    "new": "$t = C.new(v => 1);",
}

ORDER = [
    "empty",
    "int_add",
    "int_cmp",
    "str_concat",
    "array_read",
    "array_store",
    "array_push_pop",
    "hash_read",
    "hash_store",
    "sub_call",
    "method_call",
    "attr_read",
    "new",
]


def script(op, n):
    return "\n".join(
        [
            PREAMBLE.get(op, ""),
            "my $t = 0;",
            "my int $i = 0;",
            f"while $i < {n} {{",
            "    " + BODY[op],
            "    $i = $i + 1;",
            "}",
            'say "done $t";',
        ]
    )


def run(cmd, path, reps=3):
    best = None
    for _ in range(reps):
        t0 = time.perf_counter()
        r = subprocess.run(cmd + [path], capture_output=True, timeout=600)
        t1 = time.perf_counter()
        if r.returncode != 0:
            return None, r.stderr.decode()[:200]
        d = t1 - t0
        best = d if best is None else min(best, d)
    return best, None


def measure(cmd, op, n0, tag):
    """-> ns per iteration, doubling N until the 2N-N delta is measurable."""
    n = n0
    for _ in range(6):
        p1 = f"{SCRATCH}/{tag}_{op}_1.raku"
        p2 = f"{SCRATCH}/{tag}_{op}_2.raku"
        open(p1, "w").write(script(op, n))
        open(p2, "w").write(script(op, 2 * n))
        t1, e1 = run(cmd, p1)
        if e1:
            return None, e1
        t2, e2 = run(cmd, p2)
        if e2:
            return None, e2
        delta = t2 - t1
        if delta >= 0.15:
            return delta / n * 1e9, None
        n *= 4
    return delta / n * 1e9, None


def main():
    os.makedirs(SCRATCH, exist_ok=True)
    engines = [
        ("mutsu", ["./target/release/mutsu"], 200_000),
        ("raku", ["raku"], 2_000_000),
    ]
    results = {}
    for name, cmd, n0 in engines:
        results[name] = {}
        for op in ORDER:
            ns, err = measure(cmd, op, n0, name)
            results[name][op] = ns
            print(f"{name:6} {op:16} {'ERR '+err if err else f'{ns:9.2f} ns'}", flush=True)

    base_m = results["mutsu"]["empty"]
    base_r = results["raku"]["empty"]
    print()
    print(f"{'operation':16}{'mutsu ns':>12}{'raku ns':>12}{'ratio':>9}")
    print("-" * 49)
    for op in ORDER:
        m, r = results["mutsu"][op], results["raku"][op]
        if m is None or r is None:
            continue
        if op == "empty":
            print(f"{op+' (loop)':16}{m:12.1f}{r:12.1f}{m/r:8.0f}x")
            continue
        mo, ro = m - base_m, r - base_r
        ratio = f"{mo/ro:7.0f}x" if ro > 0.5 else "      -"
        print(f"{op:16}{mo:12.1f}{ro:12.1f}{ratio:>9}")


if __name__ == "__main__":
    sys.exit(main())
