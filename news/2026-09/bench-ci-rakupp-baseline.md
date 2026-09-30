# The bench CI measures Raku++ as a second reference implementation

Until now every benchmark was compared against one engine, Rakudo, measured on
the same runner in the same job. The bench CI now also measures
[Raku++](https://github.com/ash/rakupp) (`rakupp`), an independent from-scratch
C++ implementation of Raku, the same way: one warmup and seven timed runs per
benchmark, median recorded.

- `scripts/bench-ci.sh` appends two columns to every row, `rakupp_median_s` and
  the mutsu/rakupp ratio. They come after the existing six, so a reader of the
  first six columns sees the same row; both are `NA` when no `rakupp` binary is
  on `PATH` (override with `RAKUPP_BIN`), which is what a local run without it
  prints.
- `.github/workflows/bench.yml` installs a pinned release (v5.0.1, the Linux
  x86_64 archive, checked against its published SHA-256), best-effort like the
  Rakudo download. The pin keeps the baseline from moving silently: bump
  `RAKUPP_VERSION` by hand.
- `bench-history.tsv` on `bench-data` gains `rakupp_median_s`,
  `ratio_mutsu_over_rakupp` and `rakupp` (the engine's `--version`) after
  `rakudo`. The header is migrated in place on the first run; existing rows are
  not rewritten and simply have no rakupp point.
- The trend dashboard (`bench-trend.html`) offers a **ratio vs rakupp** metric
  once the history has a rakupp column (`#metric=ratiopp`).

As with Rakudo, `bench-yaml-parse` records `NA` for rakupp: the runner has no
YAMLish installed for either engine.
