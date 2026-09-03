# The late-August call-path slowdown, minus the ADR-0037 step, is still ~24% on `bench-fib`

Between 2026-08-19 and 2026-08-31 a broad set of call-path-shaped benchmarks got
slower while an unrelated set got faster. Daily medians of the bench-CI series
(`git show origin/bench-data:bench-history.tsv`), 2026-08-17..19 vs
2026-08-31..09-02:

| regressed | | improved | |
| --- | --- | --- | --- |
| `hash-access+jit` | +37% | `bench-array+jit` | −40% |
| `bench-grammar-parse+jit` | +36% | `bench-ctor+jit` | −33% |
| `bench-fib+jit` | +34% | `bench-yaml-parse+jit` | −21% |
| `bench-tak+jit` | +23% | `method-call+jit` | −10% |
| `bench-hash+jit` | +19% | `bench-class+jit` | −9% |

`bench-startup` is flat over the window (+0.4 ms), so this is not startup cost
and is unrelated to `todo/perf/interpreter-new-is-expensive-and-retains-memory.md`.

The first bad commit for the largest step was found and fixed —
`news/2026-09/adr0037-routine-frame-push-intern-cost.md`: #6720 (ADR-0037
Slice 1) added a `RoutineFrame` push to the light call paths whose five
per-call `Symbol::intern`s cost 26% of `bench-fib`. That is now ~3%.

**What remains:** with that fix in place, a local release build is still
~24% slower than a build of `af7c5d6eb4d9` (2026-08-19) on `bench-fib`
(0.1497 s vs 0.1190 s, interleaved medians of 15 on an idle box). `bench-hash`,
`hash-access`, `bench-string` and `bench-grammar-parse` show a similar residual.
So at least one more regression landed in the same two weeks.

## How to continue

Repeat exactly what found the first one — the method is the reusable part:

1. Build a reference binary at `af7c5d6eb4d9` and one at `main`, and confirm the
   gap reproduces locally. It does; the bench-CI per-commit rows do **not**
   attribute anything, because for a sub-0.2 s benchmark they are bimodal
   (`bench-fib+jit` alternates ~0.17 s / ~0.25 s on adjacent commits). Do not
   try to bisect the CI series itself.
2. `git bisect start --first-parent main <good>` with a `bisect run` script that
   does `cargo build --release` and times `benchmarks/bench-fib.raku` with
   `MUTSU_JIT=on`, median of 7, exiting non-zero above a threshold placed
   between the two known medians. ~3.5 min per step, ~9 steps over the 570
   first-parent commits in the window.
3. Set the threshold to sit just above the *post-#6720* level (say 0.155 s
   against a `main` at ~0.166 s) so the bisect finds the next step rather than
   re-finding #6720. Note the bisect must run on a branch that already contains
   the #6720 fix, or apply it per-step, otherwise every commit after #6720 is
   "bad" for the already-known reason.
4. Once a commit is identified, confirm with a scaffolded build that
   env-gates the suspect work, so the attribution is a measured delta rather
   than an inference from the diff.

Run this solo, on an idle box (`uptime`, `pgrep -c -x rustc`), and interleave
the A/B binaries rather than measuring them in sequence — a non-interleaved
comparison of two separately-built binaries drifted enough here to invert a
5% result. Per `todo/README.md` and CLAUDE.md, any number that ends up in a
document comes from the bench CI, not from the session's local runs.
