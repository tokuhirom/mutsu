---
name: perf-tuning
description: Profile mutsu and land a measured optimization — the warm/cold precompilation trap that makes a 650M-instruction difference appear or vanish, the callgrind recipe and its caller-count aggregation, which counter to trust for which question, and how to report a win honestly. Use when working a todo:perf issue, when a benchmark is slower than expected, or before quoting any performance number in a PR, news entry or issue.
metadata:
  short-description: Measure, optimize and report a perf change
---

# Performance tuning

Most of this skill is about **not fooling yourself**. The optimizations are the easy part;
every expensive mistake in this repo's perf history has been a measurement mistake.

## 0. The measurement contract — read before running anything

Four rules. Breaking any one of them has produced a published number that was wrong.

### Discard the first run after a build

**Rebuilding — or merely `touch`ing — `target/release/mutsu` invalidates the module
precompilation cache, and the next run recompiles the module under test.** On
`tmp/bench_json.raku` with JSON::Fast that is **~650M instructions, 34% of the run**:

| | Ir |
| --- | ---: |
| run 1 (cold) | 2,542,308,493 |
| run 2 | 1,892,071,969 |
| run 3 | 1,892,265,123 |

Same binary, same input, nothing touched between runs. Natively it is ~70-100ms on a ~260ms
benchmark. A warm/cold mismatch is worth more than most individual wins, so it is the easiest
way to measure an improvement that does not exist — or to miss one that does. It is how
`news/2026-09/a-rejected-parse-alternative-stops-allocating-its-message.md` came to claim a
"fixed 668M cost in the first `from-json` call" that is really 24M.

So: **run the workload twice and quote the second**, or state explicitly that a figure is cold
and compare it only against another cold one. Every A/B needs a rebuild, so *both* sides are
cold if you take one run each — that is like-for-like and valid, but the denominator then
includes a compile pass the steady state does not do, and any "% of the program" you quote
from it is against an inflated total.

`--profile profiling` is a *separate build* from `--release`, so switching between them
invalidates the cache too.

### Re-baseline from the current `main`, every time

`main` moves several times an hour. Reusing a previous slice's "after" run as the next slice's
"before" attributes other people's changes to yours. Always:

```sh
git stash push -u -m ab && cargo build --profile profiling && <measure>   # baseline
git stash pop && cargo build --profile profiling && <measure>             # after
```

### Trust instruction counts; distrust this box's wall clock

Per-code-path instruction counts under callgrind are deterministic and reproduce to the byte
across different binaries. Whole-program totals still move a few percent run to run (per-process
`HashMap` seeds change probe counts), and wall clock drifts *between sessions* by more than any
single win — 2.60s and 1.92s for the same benchmark an hour apart. Only paired A/B runs taken
minutes apart are worth quoting, and a change under ~2% will not be visible in them at all.

### A number with no baseline beside it is not a measurement

Never quote an absolute from one session against an absolute from another.

## 1. Pick the question, then the tool

| Question | Tool |
| --- | --- |
| Where do the instructions go? | `callgrind` + `cg-summary.py` (§2) |
| How many times is this called? | `cg-summary.py --callers NAME` |
| How many allocations, and from where? | `cg-summary.py --allocs`, or `alloc_scope!` + `--features alloc-stats` |
| How many allocations does a whole benchmark make? | `scripts/bench-det.sh`'s `allocations` column (§1a) |
| Which code path actually ran? | `rust-gdb -batch` breakpoint + `bt` — never an `eprintln!` |
| How many VM events? | `MUTSU_VM_STATS=1` on the **debug** build |
| Is it faster? | averaged paired wall clock, 7+ runs each, second-run-or-later |

### Which build

| Profile | Use it for |
| --- | --- |
| `cargo build` (debug) | iterating against `MUTSU_VM_STATS` / `alloc-stats` counters |
| `cargo build --release` | wall clock, and the number that reflects what ships |
| `cargo build --profile profiling` | callgrind line attribution (release opt + debuginfo) |

**Iterate on the debug build when the metric is a counter.** `MUTSU_VM_STATS=1`'s dual-store
counters (`locals_pulls`, `env_flushes`, `env_deep_copies`, `clone_env`, ...) count VM events,
not time, so they are **byte-identical in debug and release** — and debug builds in ~30-70s
where release takes ~5min. Do not default to release just because the task is perf-related;
reserve it for the final wall-clock measurement. (`[profile.release]` sets `debug = false`,
which is why line-level profiling needs `--profile profiling`.)

### The raw `perf` CLI: path, sudo, hybrid CPU

This box's `perf` binary and its sudoers entry break every time the kernel updates. `perf` lives
under a kernel-version-specific path, and until the `7.1.5-76070105` kernel that path was
`/usr/lib/linux-tools/<uname -r>/perf` from the `linux-tools-<version>-generic` package; as of
that kernel, `perf` was split out into its own `linux-perf` package and installs to plain
`/usr/bin/perf` instead. The sudoers `NOPASSWD` rule is pinned to one exact path, so it silently
stops matching on every kernel bump. When `perf` "stops working" after a `uname -r` change,
check this before suspecting a real regression:

```sh
uname -r
which perf                     # or: ls /usr/lib/linux-tools/$(uname -r)/
sudo -n -l | grep perf         # does the NOPASSWD path match `which perf`?
apt-cache policy linux-perf    # not installed at all? sudo apt install linux-perf
```

Updating the sudoers path (`visudo`) is the maintainer's call, not something to script around.

`/proc/sys/kernel/perf_event_paranoid` on this box is `2`, so **`perf stat`/`perf record` work
without sudo for userspace-only counters** — no need to chase the sudoers entry just to take a
measurement. Fall back to `sudo perf ...` only if a run reports `<not counted>` for a
kernel-level event.

This CPU is **hybrid (P-core/E-core)**: an unqualified `perf stat -e cycles,instructions` splits
each counter into separate `cpu_atom/…/` and `cpu_core/…/` rows, and whichever core the process
didn't run on that sample prints `<not counted>`. Pin to a P-core and name the event's PMU
explicitly instead:

```sh
taskset -c 2 perf stat -e cpu_core/cycles/,cpu_core/instructions/ -- ./target/release/mutsu bench.raku
```

`perf record`/`report` for call-graph work does not work in this container at all (§7 below has
the reason) — use callgrind (§2) for anything that needs a call tree. Flat `perf stat` counter
readings are fine.

### Counting allocations: `alloc_scope!` + the `alloc-stats` feature

When the question is "how many allocations does *this region* cost" rather than "where does
wall clock go":

```sh
cargo build --release --features alloc-stats
MUTSU_ALLOC_STATS=1 ./target/release/mutsu benchmarks/bench-ctor.raku
```

`alloc_scope!("label")` (`src/alloc_stats.rs`) opens an accounting region for the rest of its
block; `alloc_scope_named!` / `alloc_scope_end!` close one early so a function can be split into
phases. The stderr report gives per-scope allocations and bytes, inclusive and exclusive of
nested scopes. Counts are exact and load-independent. **Wall clock from an `alloc-stats` build
is meaningless** — time with an ordinary release build. With the feature off (every normal build,
and CI) the macro expands to nothing and the counting allocator is not installed, so call sites
are free to leave in place.

`cg-summary.py --allocs` answers the same question from a callgrind run without a rebuild, at
whole-function granularity; `alloc_scope!` is what you want for a region smaller than a function.

### 1a. Whole-benchmark allocation counts, tracked per commit

`scripts/bench-det.sh` prints `benchmark  instructions  allocations`, and the bench CI appends both
to `bench-det-history.tsv` on `bench-data` (the trend page has an `allocations` metric button).
The allocation count is the call count of the libc allocator entry points, read out of the *same*
callgrind profile as the instruction count — so it costs nothing extra, and it is at least as
reproducible as the instruction count. Measured over repeated runs of one binary: `bench-hash` gave
**237,793 three times, to the allocation**, and `bench-grammar-parse` 38,862 / 38,858 / 38,861 —
0.010%, against 0.016% for its own Ir in the same runs. (A benchmark that reproduces exactly is the
common case; where it does not, the cause is the same per-process `HashMap` seeding that moves Ir,
reaching the allocator through a string or table whose growth depends on iteration order.) That
makes it the right acceptance criterion for a change whose whole point is to allocate less, and the
right thing to check when a slice removes instructions and you want to know whether it removed the
allocation too.

**§0's warm/cold rule applies to it, and it shows the effect more loudly than Ir does.** A two-run
sample put eight of ten series within 0.01% — and `bench-json-fast` at 1,777,030 then 1,311,894,
**-26%**, purely from populating the module precompilation cache. Same trap, same benchmark, same
cause as the 650M Ir in §0. The recorded CI series is unaffected (that job builds fresh every time,
so it is cold every time, consistently); a local A/B is not, so discard the first run there too.

Three limits, all of them real:

- **It counts calls, not bytes.** The same number of larger blocks reads as unchanged. Bytes need
  memcheck: `BENCH_DET_BYTES=1 scripts/bench-det.sh` adds a column for it, at roughly 3x the wall
  time, which is why CI does not.
- **It is whole-process**, so a C dependency's allocations are in there too, and it cannot say
  *where* an allocation came from. That is what `cg-summary.py --allocs` and `alloc_scope!` are for.
- **Fewer allocations is not automatically faster.** Same rule as instruction counts: it localizes
  a change, the wall-clock series says whether it mattered.

The extractor reads callgrind's own output format, and its failure mode is a silent undercount, so
`make check-bench-det` (`scripts/bench-det.sh --self-test`, in the CI checks job) pins it against a
fixture with a known answer. If you ever distrust an allocation number, run that first.

Locally, name the files you care about — no argument measures all 26, which is four minutes you
rarely need:

```sh
cargo build --release
scripts/bench-det.sh benchmarks/bench-hash.raku      # both lanes: MUTSU_JIT=off and +jit
```

## 2. The callgrind recipe

```sh
cargo build --profile profiling          # release opt + debuginfo; needed for line attribution
valgrind --tool=callgrind --callgrind-out-file=tmp/cg.out \
  --cache-sim=no --branch-sim=no \
  ./target/profiling/mutsu tmp/bench_json.raku tmp/licenses-100.json
callgrind_annotate --tree=caller --threshold=99 tmp/cg.out > tmp/ann.txt
.agents/skills/perf-tuning/cg-summary.py tmp/ann.txt
```

A plain `--release` binary has no debuginfo and `callgrind_annotate` can only say
`???:<function>`. Callgrind is ~50x slower than native, so use the 100-record input to explore
and the 727-record one to confirm a win scales.

**`callgrind_annotate` splits one function across every file its inlined code came from**, so
the hottest function in the interpreter appeared eight separate times at 2.87%, 0.75%, 0.57% …
and never once at its real 5.71%. `cg-summary.py` merges those rows; read raw
`callgrind_annotate` output only for per-source-line attribution.

```sh
cg-summary.py tmp/ann.txt                      # top self-cost, merged
cg-summary.py tmp/ann.txt --callers has_type   # who calls it, and how many times
cg-summary.py tmp/before.txt tmp/after.txt     # A/B, biggest movers first
cg-summary.py tmp/before.txt tmp/after.txt --allocs   # A/B allocation counts by site
```

## 3. Call count, not per-call cost

The most valuable column is usually `--callers`, not self cost. `lookup_in_package_chain` stayed
the #1 function *after* a change that made each call cheaper, because the call count was the
problem: 648,000 walks per 100 parsed records, essentially all of them misses. The fix took it
to **zero entries**, not to a lower per-call cost.

Ask "why is this called at all?" before "why is this call slow?".

## 4. The two shapes almost every win here has had

**A structure that cannot answer a miss without visiting everything.** The package-keyed symbol
tables are walked by *package* while the answer is decided by the *name*; the type registry
answered "is there a mangled key for this name?" with a linear scan of every key in four maps.
Both were fixed by adding the index the query actually wanted — exactly, not approximately — and
both caches are invalidated by the `DerefMut` that is the sole path to `&mut` on the table they
summarise, so they cannot drift. **Prefer a cache whose invalidation is structurally impossible
to forget over one maintained at N call sites.**

**Copying a string that is already in hand.** `Self::const_str(code, idx).to_string()` on an
opcode operand (`code` is not borrowed from `self`, so `&str` works); `PError::expected(what:
&str)` doing `vec![what.to_string()]` when 434 of 442 call sites pass a literal;
`current_package()` cloning a `String` the atomic symbol mirror already answers as `&'static
str`. Also `str::contains("::")` / `split_once('[')`, which build a `StrSearcher`/`CharSearcher`
for a fixed two-byte needle — `src/runtime/utils/str_scan.rs` has the byte scans.

## 5. Slice it, and do not close the issue

One coherent change per PR, each with its own measurement. A `todo:perf` issue's headline
problem usually needs an ADR; the bounded slices under it do not, and landing four of them beat
waiting for the big one — **when the slices can actually reach the target**. Section 5a is the
case where they cannot, and it is the more common one once the obvious wins are gone.

**Do not close the issue from the PR that does one slice.** Deciding a perf issue's scope is
satisfied is the maintainer's call. (Got this wrong on #8673 — closed it via `Closes` in a PR
that merely removed one of its costs.)

## 5a. State the required multiple first, or slicing becomes avoidance

**Before the first slice, write down the multiple the goal needs, and check that the plan's
slices multiply to it.** A goal stated as a ratio — "get `JSON::Fast` under 10x rakudo", from
59x — needs **6x**. A slice worth 3% contributes 1.03. Sixty of them, with no interaction,
would be 5.9. That arithmetic is the whole decision, and it takes one line to do.

Skipping it has a specific failure mode, and it is seductive rather than obviously wrong:

1. You profile, and the top entry is real.
2. You fix it soundly, with a measurement, and it lands. It is worth 1-4%.
3. You profile again. A different entry is on top. Go to 2.

Every step is defensible. The sequence goes nowhere, and it feels like progress the entire time
because each PR is green, measured, and general. **The tell is that your own write-up keeps
concluding "the structure is the cost" while your next commit keeps not changing the structure.**

This happened over one long session (2026-09-20). The analysis was right every time and was
ignored every time:

| landed | measured |
| --- | ---: |
| six name-resolution slices against #8830 | 2,975,774,664 → 1,843,258,057 Ir, and the last removed **65.8% of the profile's single largest entry for 4.3% overall** |
| #8879, the `DESTROY` sweep | -3.9% on a loop |
| #8886, the dispatch chain's **most expensive probe**, removed outright | **3.1%** of a method call |
| #8890, `as_str` instead of `resolve` on the native dispatch path | **393** instructions of 22,528 |
| pure-tag gating of the `native_method_0arg` prologue | **1.17%**, reverted |

The write-up for #8886 said, in its own PR body, *"there is no ordering of probe removals that
reaches a method call worth calling fast, because the chain is the design"* — and the next two
commits removed another cost from the chain.

Three rules follow.

**Count the layers before you optimize one.** `@a.elems` is 8,314 instructions and *fifteen*
layers of 100-900 each. Knowing that first would have ruled out the last three rows of that
table. `callgrind_annotate --tree=caller` plus `cg-summary.py` gives you the list in one run;
read the whole list before picking.

**An existing per-layer cache is evidence against adding another one.** When every layer already
carries a memo from a previous campaign — `native_lever_a_user_override_sym` is address-keyed to
avoid an intern and cites two campaigns in its doc comment, and still costs 87 instructions on a
*hit*, twice per call; `vm_call_method_compiled_cache.rs` is 849 lines of caching; `Symbol::intern`,
`MetaNs::key`, `type_meta_key_cache` and `capture_candidates` all memoize — and the path is still
slow, the answer is not a sixteenth memo. It is a cache **in front of** the layers instead of
inside each of them. Fifteen layers of cache lookups is what "make each layer cheap" converges to.

**A negative result is a deliverable; a small win is not a reason to stop.** Reverting the
tag-gating experiment and writing down *why* (7 of 57 decodes bought 1.17%; all 57 caps at 12%)
was worth more than landing it would have been. Publish that, then go and do the structural
change.

## 6. Reporting

- **Say so when wall clock does not move.** Two slices in this campaign cut instructions and
  allocations measurably with no stopwatch effect, for reasons that were worth stating. "This
  does strictly less work" is an honest claim; "this is faster" was not.
- Give the deterministic counts as the evidence and the wall clock as the reading, with the run
  count.
- When a percentage's denominator includes something the steady state does not do, say that too.
- **Numbers in `PERFORMANCE.md` / `PLAN.md` / `news/` come from the bench CI, not local runs.**
  `git show origin/bench-data:bench-history.tsv` — appended on every main push, median of 7 runs
  plus a same-runner raku ratio that normalizes runner speed — citing the main commit hash the
  row belongs to. The `<bench>+jit` rows are the JIT-on series (the default since J5,
  2026-07-13); the plain rows pin `MUTSU_JIT=off` as the interpreter baseline. Local A/B is fine
  for PR bodies and in-flight decisions, but it drifts with thermals and binary layout (±5% is
  common), so it is not the source of truth for a document.
- If a published number turns out wrong, correct it in place and leave the mistake visible
  rather than deleting it — the correction is worth more than the original claim was.

## 7. Traps that have each cost a session real time

- A shell chain like `make roast 2>&1 | tail -25 && echo OK` reports `tail`'s status. Both
  suites set `-o pipefail` internally; read the **exit code**, then `tmp/make-*.log` with Grep
  for *which* file failed — never re-run a suite to see its output.
- `make roast` is red in a container for two documented `uid 0` files; check
  `docs/agent-environments.md` by name **and** by exact subtest number before shipping.
- Rustdoc resolves intra-doc links from the enclosing *module*, so ``[`Interpreter::foo`]`` in a
  new module fails `make lint` with nothing local warning you. Run the full `make lint`, not just
  the pre-commit hook.
- Pushing to a PR branch cancels its in-flight CI run, and the aggregator jobs report that
  cancellation as `failure`. Check the run's `conclusion` before treating it as a real break.
- `perf --call-graph` has never worked in this container — a stale `/root/.debug`
  build-id store breaks `dwarf`, and `fp` yields garbage stacks. Use callgrind.
