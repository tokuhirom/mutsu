# LTM NFA runs reuse their working storage

Since ADR-0127 every LTM measurement simulates the pattern's NFA, and a
grammar parse makes tens of thousands of those runs (34,568 on
`bench-grammar-parse-big`). Each run allocated its tables afresh: the seen set
(two node-sized arrays, filled with a sentinel), the interned call-stack
table, the enclosing-names list and the thread queues. That was the +48%
allocation step #10008 found on `bench-grammar-parse-big` at the #9677 merge.

A run now takes its working storage from a per-thread pool
(`regex_ltm_nfa_scratch`) and hands it back when it ends; nested runs (a
`:m` or scoped region, a runtime-resolved call) each take their own. The seen
set is stamped instead of filled, so a reused one needs no clearing at all:
a slot counts only when its stamp is the current position's, and stamps only
grow. Storage that grew past 4096 stacks is dropped rather than pooled.

Measured with `scripts/bench-det.sh` (warm runs, same profile build, paired
A/B; interpreter lane, the `+jit` lane agrees):

| benchmark | allocations before | after | instructions before | after |
|---|---:|---:|---:|---:|
| `bench-grammar-parse-big` | 405,060 | 229,664 (-43%) | 287,335,136 | 242,472,860 (-15.6%) |
| `bench-grammar-parse-deep` | 39,803 | 37,029 (-7.0%) | 28,201,863 | 27,442,693 (-2.7%) |
| `bench-grammar-parse` | 43,083 | 40,047 (-7.0%) | 30,614,182 | 29,784,565 (-2.7%) |

`bench-grammar-parse-big` is now below its pre-#9677 baseline of 274,644
allocations, so the measurement unification no longer costs allocations at
all; the simulation itself is unchanged, so ADR-0127's ranking is too.
