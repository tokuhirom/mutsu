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
| Where do the instructions go? | `callgrind` + `cg-summary.py` (below) |
| How many times is this called? | `cg-summary.py --callers NAME` |
| How many allocations, and from where? | `cg-summary.py --allocs`, or `alloc_scope!` + `--features alloc-stats` |
| Which code path actually ran? | `rust-gdb -batch` breakpoint + `bt` — never an `eprintln!` |
| How many VM events? | `MUTSU_VM_STATS=1`, identical in debug and release, so iterate on the **debug** build |
| Is it faster? | averaged paired wall clock, 7+ runs each, second-run-or-later |

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
waiting for the big one.

**Do not close the issue from the PR that does one slice.** Deciding a perf issue's scope is
satisfied is the maintainer's call. (Got this wrong on #8673 — closed it via `Closes` in a PR
that merely removed one of its costs.)

## 6. Reporting

- **Say so when wall clock does not move.** Two slices in this campaign cut instructions and
  allocations measurably with no stopwatch effect, for reasons that were worth stating. "This
  does strictly less work" is an honest claim; "this is faster" was not.
- Give the deterministic counts as the evidence and the wall clock as the reading, with the run
  count.
- When a percentage's denominator includes something the steady state does not do, say that too.
- Numbers that go into `PERFORMANCE.md` / `PLAN.md` / long-lived docs come from the **bench CI**
  (`git show origin/bench-data:bench-history.tsv`, median of 7 with a same-runner raku ratio),
  citing the main commit. Local A/B is for PR bodies and in-flight decisions only.
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
- `perf --call-graph` has never worked in this container. Use callgrind.
