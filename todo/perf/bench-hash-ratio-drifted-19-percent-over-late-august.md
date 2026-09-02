# `bench-hash` got ~20% slower over 2026-08-19 → 2026-08-31, cause unattributed

Read from the bench CI series (`git show origin/bench-data:bench-history.tsv`),
daily means of the same-runner `ratio_mutsu_over_raku` column so runner speed is
normalized out:

| date | `bench-hash+jit` ratio | `array-ops+jit` ratio |
| --- | --- | --- |
| 2026-08-19 | 0.141 | 0.373 |
| 2026-08-23 | 0.149 | 0.369 |
| 2026-08-25 | 0.156 | 0.381 |
| 2026-08-27 | 0.157 | 0.381 |
| 2026-08-28 | **0.168** | 0.387 |
| 2026-08-31 | 0.169 | 0.391 |

Absolute daily means confirm it is mutsu, not the runner: mutsu 0.0339s →
0.0400s (+18%) while raku went 0.2441s → 0.2365s (raku got slightly *faster*).
`array-ops+jit` drifted much less (+5%).

## What it is NOT

ADR-0040's element-store itemization was the obvious suspect (slice 1 landed
2026-08-21, slice 2 on 2026-08-27, and the two step-ups are one day after each).
Measured with a `rust-gdb` breakpoint on a 200-insert/100-delete hash script:

- `Value::hash` (slice 4b's construction hook): **3 calls** for 300 hash
  operations — hash insert and delete mutate in place, so the construction scan
  is not on this benchmark's hot path at all.
- `Value::itemize_for_element_store` (slice 1's element-store hook): **200
  calls**, exactly one per `%h{k} = v`, each a single discriminant test on an
  `Int` that returns the value unchanged.

One tag test per store cannot account for +18%. So the drift is something else
that landed in the same two weeks — dozens of unrelated PRs a day.

## What to do

Bisect the bench-CI series per commit rather than per day. The per-commit rows
are noisy (0.023–0.044s on 2026-08-27 alone), so a single row proves nothing:
pick candidate commits from the daily means, then A/B `benchmarks/bench-hash.raku`
locally on release builds of those exact commits, several runs each, on an
otherwise idle box.

`bench-hash.raku` is 10000 string-keyed inserts, 10000 lookups, `.keys`/`.values`,
then 5000 `:delete` — so the suspects are the string-key hashing path, the
`HashData` copy-on-write, and `:delete`.

## Provenance

Observed by ADR-0040 slice 5 (2026-09-02) while answering the ADR's §5.2 perf
question. The §5.2 answer itself is "no ADR-0040 slice shows a step" — this
ticket records the unrelated drift the check surfaced so it does not evaporate.
Per CLAUDE.md, work it in a dedicated solo profiling session, and take document
numbers from the bench CI rather than that session's local runs.
