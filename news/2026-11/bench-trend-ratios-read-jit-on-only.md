# Bench trend reads every ratio on the JIT-on series only

The bench trend page (`scripts/bench-visualize.py`) already took its headline ratio from the
`+jit` series, but it still fell back to the JIT-off series when a benchmark had no JIT-on rows,
and it drew the JIT-off line on the ratio charts and its ratio in the table. Losing to rakudo or
rakupp with `MUTSU_JIT=off` is not a perf gap (the JIT is on in the shipped binary), so the
ratio charts, the win/loss colouring, the sort order, the table and `--report` now read
`@section+jit` (else `+jit`) only. The JIT-off series stays on the seconds and counter charts as
mutsu's own JIT-on vs JIT-off comparison, with no ratio.

Each ratio tab now shows only its own reference: the "ratio vs rakudo" tab drops the rakupp
ratios, the rakupp-only notes (such as rakupp answering JSON::Fast natively) and the rakupp table
column, and "ratio vs rakupp" drops the rakudo ones, so headline numbers, colouring and table
columns follow the selected tab.

`docs/benchmarks.md` and `docs/rakupp-gap-survey.md` state the rule; the site e2e test pins
that a ratio chart draws no JIT-off line.
