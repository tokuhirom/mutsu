# The perf-tuning skill reads ratios on JIT-on rows only

The `perf-tuning` skill now states the rule next to its bench-CI citation guidance: ratios
against rakudo and rakupp are read on the `+jit` / `@section+jit` rows, and a JIT-off row is never
a perf gap. `docs/benchmarks.md` carries the full rule and the bench trend page reads it.
