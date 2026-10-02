# Benchmarks and measurement

How the benchmark suite is built and how to read the numbers it produces. Current numbers live in
the bench CI history (`git show origin/bench-data:bench-history.tsv`), not in any document — a
table copied into a file goes stale on the next main push. The procedure for profiling and landing
a perf change is the [`perf-tuning`](../.agents/skills/perf-tuning/SKILL.md) skill.

## The suite

Benchmarks live in `benchmarks/`. Run all of them locally with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

Every `benchmarks/*.raku` file is measured by the bench CI automatically (two configurations
each — `<name>` pins `MUTSU_JIT=off` as the interpreter baseline, `<name>+jit` is the default
JIT-on configuration — with wall clock, simulated instruction counts and heap allocation counts;
see `scripts/bench-ci.sh` and `scripts/bench-det.sh`), so adding a file is all it takes to add a
series. Each row is the median of 7 runs plus a ratio against `raku` measured on the same runner in
the same job, and against Raku++ (`rakupp`) as a second reference.

### Adding a benchmark

- Keep it **deterministic** and **self-checking** (print a checksum).
- Keep it in the **0.1-0.4 s range** on a release build: the deterministic series resolves ~0.1%,
  while the wall-clock series needs the file to be well clear of the ~8 ms startup without paying
  90x for it under callgrind. A file that deliberately breaks the range says why in its header
  (e.g. `bench-json-fast-spdx.raku`).
- Isolate **one mechanism** per file and say in its header what it measures and why.

### Section and warm series

When the operation a benchmark exists for is small next to either interpreter's startup and module
loading, the whole-script ratio reads near 1 no matter how slow the operation is. Such a benchmark
prints `bench-section-seconds: <s>` (its own in-process timing) and the bench CI records an extra
`<name>@section` / `<name>@section+jit` series from it, with raku's in-process time as the ratio's
denominator.

The regex and grammar files ([#9916](https://github.com/tokuhirom/mutsu/issues/9916)) report a
**warm** section: they run the workload twice untimed and time the third run. Read the `@section`
rows when comparing against rakudo — a whole-script row charges rakudo its JIT warm-up, which
ADR-0099 §2.1 measured as most of a 4.2x headline.

Regex ratios depend on subject size: on ~100-character subjects mutsu measures 0.2-0.4x rakudo,
and the same operations measure 1.0x and worse on multi-hundred-kilobyte ones. A regex ratio
without its subject size says nothing.

## Reading the numbers

- **Numbers in `PLAN.md`, `news/`, issues and PR descriptions come from the bench CI**, citing the
  main commit hash the row belongs to. A profile (`docs/profiler.md`) tells you *where* to look,
  not *how fast* something is.
- Local `perf stat -r5` under `taskset` is fine for in-flight A/B decisions, but drifts with
  thermals and binary layout (±5%); check for stray `mutsu` processes before measuring. Prefer the
  deterministic counts (instructions, allocations) as evidence.
- Always use a `--release` build. raku times include ~120-170 ms startup, mutsu's ~4-8 ms.
- **The raku ratio does NOT normalize the CI runner's host class, and several benchmarks cannot
  resolve a small change at all.** The bench CI runs on a *bimodal* runner pool. Splitting
  main-push rows by `bench-startup` `mutsu_median_s` (`< 6 ms` = fast host) put mean `bench-ctor`
  **ratio** at 0.62 on fast hosts against 0.88 on slow ones — the effect is in the ratio column,
  not only in the seconds column. It is specific to the long, allocation-heavy benchmarks:
  `hash-access` and `fib` hold steady across the same swings. So `bench-ctor`, `method-call`,
  `bench-class`, `time-parts`, `debug-guard`, `bench-mandelbrot` and the *interpreter* rows of
  `bench-fib`/`bench-tak` **cannot resolve anything smaller than ~30%**, and an apparent move on
  one of them is usually a change in host-class composition between the two windows compared.
  Always quote the benchmark name *and its noise class* alongside any number.
