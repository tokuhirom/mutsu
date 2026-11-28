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
the same job, and against Raku++ (`rakupp`) as a second reference. What the rakupp ratio
is made of, row by row, and which of its rows are not engine against engine (rakupp answers
JSON::Fast natively) is in [rakupp-gap-survey.md](rakupp-gap-survey.md).

### Which rows are compared against rakudo and rakupp

**Ratios against rakudo and rakupp are read on the JIT-on rows only** (maintainer decision,
2026-11-28): `<name>@section+jit`, or `<name>+jit` for a benchmark with no section. The JIT is on
by default in the shipped binary (ADR-0001 §7); the plain `<name>` / `<name>@section` rows pin
`MUTSU_JIT=off` (`scripts/bench-ci.sh`) and are mutsu's own interpreter baseline. **A JIT-off row
is never a perf gap**: losing to rakudo or rakupp there is not a defect, and a `todo:perf` issue's
close condition or headline number is stated on the `+jit` row. The JIT-off series is kept for
mutsu's own JIT-on vs JIT-off comparison (seconds and counter charts), never as a ratio.

### Adding a benchmark

- Keep it **deterministic** and **self-checking** (print a checksum).
- Give it a **warm timed section** (next heading): the workload is a `sub workload()` that returns
  its checksum line; two untimed runs, then the timed third, printing `bench-section-seconds:`.
  Only `bench-startup` is exempt, because it measures startup on purpose. The checksum line must
  be identical under mutsu and `raku`.
- Keep it in the **0.1-0.4 s range** on a release build: the deterministic series resolves ~0.1%,
  while the wall-clock series needs the file to be well clear of the ~8 ms startup without paying
  90x for it under callgrind. A file that deliberately breaks the range says why in its header
  (e.g. `bench-json-fast-spdx.raku`).
- Isolate **one mechanism** per file and say in its header what it measures and why.

### Section and warm series

**The `@section` ratio is the headline.** A whole-script row charges every interpreter its startup
and module loading — raku's ~120-170 ms against mutsu's ~4-8 ms — so a light benchmark's
whole-script ratio sits near 1 however slow the operation is. Every benchmark therefore prints
`bench-section-seconds: <s>`, its own in-process timing of the workload, and the bench CI records
`<name>@section` / `<name>@section+jit` series from it, with raku's (and rakupp's) in-process time as
the ratio's denominator. The whole-script `<name>` / `<name>+jit` rows are still recorded, as a
**secondary** series. `bench-startup` is the one benchmark with no section: it *is* the startup
metric.

Every section is **warm**: the workload runs as `workload()` twice untimed and a third time timed,
following [`bench-regex-match.raku`](../benchmarks/bench-regex-match.raku)
([#9916](https://github.com/tokuhirom/mutsu/issues/9916), ADR-0099 §6):

```raku
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;
```

Read the `@section+jit` rows when comparing against rakudo — a whole-script row charges rakudo its JIT
warm-up as well, which ADR-0099 §2.1 measured as most of a 4.2x headline. Under
`scripts/bench-det.sh` (`BENCH_DET=1`) the workload runs once, so the deterministic series keeps a
single execution per file; those series are whole-script (startup included) — see
[the deterministic series](#the-deterministic-series).

The section series of the benchmarks that gained a section late (every file but the 13 regex,
grammar and module ones) **start at one commit**
(`news/2026-11/benchmarks-headline-the-section-ratio.md`): the trend page marks that commit with a
dotted line, because before it there is no section point rather than a drop.

Regex ratios depend on subject size: on ~100-character subjects mutsu measures 0.2-0.4x rakudo,
and the same operations measure 1.0x and worse on multi-hundred-kilobyte ones. A regex ratio
without its subject size says nothing.

### The deterministic series

`scripts/bench-det.sh` runs each file once under callgrind. Its instruction and allocation counts
include mutsu's own startup, which is a constant floor under every file.

For each file and JIT mode `bench-det.sh` prints, next to the raw `<name>` / `<name>+jit` rows,
`<name>@net` / `<name>@net+jit`: the raw count minus the count of an **empty script** measured in
the same job, in the same JIT mode, by the same callgrind invocation (clamped at 0). The raw rows
are unchanged, so the older series continues; the trend page offers the net counts as the
*instructions (net)* and *allocations (net)* metrics and does not draw `@net` rows as cards of
their own. Net exists because a light benchmark's raw count is mostly the interpreter's startup
floor, so a 10% change in its own work barely moves it.

Why a subtracted baseline and not callgrind's `--toggle-collect` around the section: the baseline
needs no interpreter function names (the first thing a refactor renames) and cannot silently
collect nothing, whereas a stale toggle yields an empty profile that reads as "the benchmark became
free". The price is stated, not hidden: the net count still contains parsing and compiling the
file, loading the modules it `use`s, and (outside `BENCH_DET`) the warm-up runs — it is "the file
minus a bare interpreter", not "the timed section alone". The wall-clock `@section` series is the
one that times the section alone.

The deterministic instruction/allocation series warms each benchmark once in both JIT modes
before callgrind, inside `scripts/bench-det.sh` itself. It therefore measures the same populated
precompilation-cache state on push-triggered CI runs that skip the wall-clock benchmark job.

## Reading the numbers

- **Numbers in `PLAN.md`, `news/`, issues and PR descriptions come from the bench CI**, citing the
  main commit hash the row belongs to. A profile (`docs/profiler.md`) tells you *where* to look,
  not *how fast* something is.
- Local `perf stat -r5` under `taskset` is fine for in-flight A/B decisions, but drifts with
  thermals and binary layout (±5%); check for stray `mutsu` processes before measuring. Prefer the
  deterministic counts (instructions, allocations) as evidence.
- Always use a `--release` build. Whole-script raku times include ~120-170 ms startup, mutsu's
  ~4-8 ms; quote the `@section` figure (startup excluded) unless startup is the point.
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

## The bench-trend dashboard

`scripts/bench-visualize.py` renders `bench-history.tsv` into `bench-trend.html` on the site
(`pages.yml`). It answers "where does mutsu lose, and by how much", not "what moved since the
previous commit", because a one-commit move on this runner pool is mostly noise:

- A card's headline is the benchmark's **`@section`** series (in-process, warm, startup excluded);
  the `foo@section` series is folded into the `foo` card rather than being a card of its own. The
  whole-script series is secondary: a "whole script: rakudo X · rakupp Y" line on the card, a dashed
  line on the ratio charts, and the table's last column. `bench-startup` (no section) and any
  benchmark whose section series has no point yet keep the whole-script series as headline. The
  dotted green line marks the commit where a card's section series starts.
- Each card, and the table, shows **vs rakudo** and **vs rakupp**: the **median of the last 10
  commits** of mutsu ÷ the reference on the JIT-on series (`@section+jit`, else `+jit`), not the
  latest point. The JIT-off series is drawn on the seconds and counter metrics only, as mutsu's own
  baseline next to `+jit`: a ratio chart, the win/loss colouring, the sort order, the table's
  ratio columns and `--report` never read it. A ratio above 1.0x is red; it is lighter (*marginal*) when a quarter or more of those
  commits were at or under parity. A ratio rakupp wins by something other than running the same
  Raku code faster (its native JSON parser, its non-PIE startup) is not coloured and does not
  count as a loss: the card shows its note instead, in every metric. Wins are not coloured,
  so the losses read at a glance.
- **Each ratio tab emphasizes only its own reference.** On "ratio vs rakudo" the cards, the red/win
  colouring, the notes and the table columns are rakudo's alone; on "ratio vs rakupp" they are
  rakupp's alone, including the "rakupp does not run JSON::Fast's Raku code" note. The seconds and
  counter metrics show both. A table sort key of the hidden reference falls back to the shown one.
- A ratio is only as stable as its baseline. Each history row records the reference versions it
  ran against (the `rakudo` and `rakupp` columns, the first line of each `--version`), and the
  page names the current ones at the top ("vs Rakudo 2026.11 · Raku++ v5.9.0"). A ratio chart
  draws a dashed vertical line where a reference version changes: every ratio steps there
  without mutsu changing. No ratio median reads across the latest change, so for the first
  10 commits after a bump a ratio reads fewer points and shows `n=` with how many.
- A losing card links the issues that track it, from
  [`benchmarks/bench-tracking.toml`](../benchmarks/bench-tracking.toml). For a losing row with no
  entry the generator warns: file a `todo:perf` issue on `lane:perf`, close condition
  "ratio <= 1.0x vs <rakudo|rakupp> on the bench CI median", then add the entry.
- The page shows no change figure, neither a single-commit delta nor a trend: run-to-run
  motion on these runners is mostly the instance, not the commit. A card's big number is the
  median of the last 10 points; the charts show the history.
- `python3 scripts/bench-visualize.py --report bench-history.tsv` prints every losing row,
  worst first, with its tracking issues, as a Markdown table for an issue or a status report.
