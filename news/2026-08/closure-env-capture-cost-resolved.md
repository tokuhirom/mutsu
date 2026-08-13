# Closure creation no longer walks the whole program's global symbol table

`todo/deep/closure-env-capture-cost.md` tracked a profiling finding from
2026-07-30: `exec_make_lambda_op` was ~11% and `capture_closure_env` ~8-10% of
total runtime on `bench-ctor`, for a benchmark whose only lambda is the
constant WhateverCode `*.flat` inside `Dist.TWEAK`, created fresh on every
construction. The filtered (non-reflective) capture path started with
`clone_env()`, which deep-clones the *entire* flattened parent-chain map —
including every global symbol reachable from the frame — before filter-copying
the kept subset and discarding the rest. In a program with N global symbols
this was an O(N) hash-insert-and-clone per closure creation, even for a
closure with zero free variables.

The ticket proposed a "two-tier capture" (an `Arc`'d, epoch-invalidated
global-tier snapshot plus a small per-creation overlay) as the sound long-term
shape, but flagged it as design work belonging with the Slice F env campaign
rather than a drive-by fix.

A simpler fix landed the same day (#5571, before this campaign's ticket was
closed out): `Env::filtered_flat` walks the tiers base-to-leaf and copies only
the entries the filter accepts, with **no intermediate whole-map clone** —
and, critically, the shared immutable `GLOBAL_BASE` tier (built-in
enum constants, added in #2589) is never walked or materialized at all; it
stays reachable through the flat env's tail lookup exactly as `flattened()`
already did. This is effectively the two-tier split the ticket asked for,
achieved by *not walking* the global tier rather than by caching it.

Re-profiled on 2026-08-14 (profiling build, `perf record --call-graph fp`, 40k
iterations of `benchmarks/bench-ctor.raku`): `capture_closure_env` and
`exec_make_lambda_op` together now account for roughly 2-3% of total runtime,
down from the ~19-21% combined figure that motivated the ticket. The remaining
cost is malloc/free, string formatting, and hashing spread across many small
per-construction operations — the territory already tracked by
`todo/tickets/bench-ctor-construction-parity.md` S2/S3, not closure capture.

No further architectural change (the two-tier `Arc` design) is warranted: the
dominant cost is gone, and the remaining ~2-3% does not justify the
lexical-hygiene and write-back risk the ticket itself flagged for that
approach.
