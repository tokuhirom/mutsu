# Why mutsu trails Raku++ on some bench rows (survey, 2026-11-25)

The bench dashboard's **ratio vs rakupp** metric puts mutsu behind
[Raku++](https://github.com/ash/rakupp) (`rakupp`, an independent C++ implementation) on 18
rows. This document records what each gap is made of, which open issues already cover each
part, and which parts no issue covers yet. It is a survey: every number below was measured once,
on the box described under [Setup](#setup), and should be re-measured before a fix is designed
on it.

## Verdict

- **The JSON rows are not engine against engine.** rakupp does not run JSON::Fast's Raku code.
  It answers `JSON::Fast::from-json` with its own C++ parser. The dashboard now says so on those
  cards (#19195). Read **ratio vs rakudo** for JSON::Fast.
- On every other row rakupp runs the same Raku code as mutsu. Three costs apply to all of them:
  1. **Startup is ~3 ms slower.** This is kernel time, not instructions: mutsu takes 4x the page
     faults because it touches ~28 MB of its own machine code to run an empty program.
  2. **Instructions run slower.** On ctor, word-count, regex-match and regex-split-subst mutsu
     retires about as many instructions as rakupp, or fewer, and is still slower: more I-cache,
     D-cache and branch misses.
  3. **malloc is a large share of the time.** Preloading jemalloc in place of glibc malloc makes
     those rows 5-15% faster.
- On top of these, a few rows have a cost of their own: module load (`use Test`, `use YAMLish`),
  one-time grammar analysis, and bless/TWEAK in `bench-ctor`.
- Three of these causes had no issue when this survey was written. They are now filed as
  #19197 (startup page faults), #19198 (the allocator) and #19199 (the regex rows' cache-miss
  profile, a sub-issue of #15737). Everything else was already tracked
  ([issue map](#existing-issues)).
- The numbers below are from `13320ba6`. Where things stood a day later, after the first
  #19197 slices and the string-allocation work (#19201), is under
  [Re-measurement](#re-measurement-2026-11-26-main-5f44cb0d).

## Setup

- mutsu: `main` at `13320ba6`, `cargo build --release`, and `--profile profiling` for valgrind.
  rustc 1.100.0. `MUTSU_JIT=off` (the plain series) unless stated.
- rakupp: v5.0.1, the release pinned in `.github/workflows/bench.yml` when this survey was made
  (bench moved to v5.9.0 on 2026-11-27; re-measure on it before designing a fix), Linux x86_64
  archive.
- Box: the 4-core remote container. Wall clock on it is noisy. Each timing below is the minimum
  or median of 5-15 runs, alternating the binaries where a comparison matters. Instruction counts
  come from `valgrind --tool=callgrind` and cache numbers from `--tool=cachegrind --cache-sim=yes
  --branch-sim=yes`. User and system time and page faults come from `wait4` rusage.
- CI figures are from `bench-history.tsv` on `bench-data`, at commit `13320ba6`.

## The rows (CI, `13320ba6`, interpreter series)

> **Reading rule (2026-11-28):** ratios against rakudo and rakupp are read on the JIT-on rows
> (`@section+jit` / `+jit`) only. This survey was taken on the JIT-off (interpreter) series, so
> its ratios overstate the gap the shipped binary has; a JIT-off row is never a perf gap. Re-read
> a row on its `+jit` series (`docs/benchmarks.md`) before designing a fix from it.

| benchmark | mutsu | rakudo | rakupp | vs rakudo | vs rakupp |
|---|---:|---:|---:|---:|---:|
| bench-json-fast-spdx@section | 0.0374 | 0.0310 | 0.0022 | 1.21 | 17.00 |
| bench-json-fast | 0.1027 | 0.2705 | 0.0154 | 0.38 | 6.67 |
| bench-json-fast-spdx | 0.0961 | 0.3447 | 0.0311 | 0.28 | 3.09 |
| bench-grammar-parse-deep | 0.0129 | 0.3177 | 0.0075 | 0.04 | 1.72 |
| bench-ctor | 0.2397 | 0.3610 | 0.1541 | 0.66 | 1.56 |
| bench-startup | 0.0077 | 0.1373 | 0.0050 | 0.06 | 1.54 |
| bench-yaml-parse-big | 0.1304 | 0.3498 | 0.0857 | 0.37 | 1.52 |
| bench-yaml-parse-big@section | 0.0302 | 0.0185 | 0.0200 | 1.63 | 1.51 |
| bench-grammar-parse | 0.0125 | 0.2427 | 0.0085 | 0.05 | 1.47 |
| string-concat | 0.0092 | 0.1527 | 0.0072 | 0.06 | 1.28 |
| bench-regex-capture | 0.1554 | 1.6068 | 0.1292 | 0.10 | 1.20 |
| bench-ripemd-rounds | 0.0583 | 0.3261 | 0.0494 | 0.18 | 1.18 |
| bench-regex-match | 0.1148 | 1.1479 | 0.0993 | 0.10 | 1.16 |
| bench-regex-capture@section | 0.0451 | 0.4321 | 0.0398 | 0.10 | 1.13 |
| bench-regex-split-subst | 0.1395 | 0.5294 | 0.1289 | 0.26 | 1.08 |
| bench-regex-match@section | 0.0312 | 0.2871 | 0.0299 | 0.11 | 1.04 |
| word-count | 0.0817 | 0.2337 | 0.0806 | 0.35 | 1.01 |
| bench-regex-assertion | 0.0737 | 0.8159 | 0.0730 | 0.09 | 1.01 |

Times are seconds. Every row below 1.08 is within the wall-clock series' noise (16-33%
commit-to-commit, see [benchmarks.md](benchmarks.md)).

## What rakupp actually runs

A `die` placed at the top of a module routine shows whether rakupp executes the Raku source:

| module | rakupp runs its Raku code? | evidence |
|---|---|---|
| JSON::Fast (`modules/JSON-Fast`) | **no** | a `die` at the top of `from-json` never fires, under both `from-json(...)` and `JSON::Fast::from-json(...)`, and the call still returns parsed data. `librakupp.so` contains `rakupp-from-json`, `&JSON::Fast::` and `JSON::Native`. |
| YAMLish (`modules/YAMLish`) | yes | a `die` at the top of `load-yaml` fires under rakupp exactly as under mutsu |
| Test | built in | rakupp lists the Test API as a built-in. `use Test;` adds nothing measurable to its startup, against ~12 ms on mutsu with a warm precomp cache. |

The rakupp binary is a non-PIE executable (`EXEC`, loaded at the fixed address `0x400000`, 24 MB,
no `R_X86_64_RELATIVE` relocations). Its own runtime (`librakupp`) is linked in statically, and
libc, libm, libpthread and libdl are dynamic. mutsu is an 85 MB PIE with ~80k relative
relocations.

## Cross-cutting cause 1: startup page faults

`bench-startup` (`say "hello"`) and an empty program:

| | instructions | user | sys | minor faults | RSS |
|---|---:|---:|---:|---:|---:|
| mutsu, empty program | 7.7M | ~2.2 ms | ~2.7 ms | ~1,200 | 29-37 MB |
| rakupp, empty program | 8.1M | ~2.4 ms | ~0.9 ms | ~300 | 8-9 MB |
| `mutsu --help` (no interpreter) | - | ~1.5 ms | ~0.6 ms | ~540 | 8 MB |

The instruction counts are the same. The difference is kernel time spent on page faults.
`/proc/<pid>/smaps` of mutsu idling in `sleep 1`:

| mapping of `target/release/mutsu` | RSS |
|---|---:|
| `r-xp` (text, ~48 MB mapped) | 27.9 MB |
| `r--p` (rodata and headers) | 4.3 MB |
| `r--p` (`.data.rel.ro` after relocation) | 1.5 MB |
| `rw-p` | 0.2 MB |

rakupp's whole binary is 6.4 MB resident. Initialising the interpreter
(`Interpreter::new`, of which `build_builtin_registry` is 48% of startup instructions) runs code
spread across most of the text segment. Each fault maps up to 16 already-cached pages
(fault-around), so ~650 extra faults are enough to bring in ~28 MB.

What this does to the rows: it is ~3-4 ms per process. That is most of the gap on
`bench-startup` and `string-concat` (string-concat itself runs *fewer* instructions on mutsu: 24.4M
against rakupp's 27.9M), and about half of it on the two grammar rows.

Startup instructions are already watched: #18956 doubled them and was fixed, and #17504 is open.
Startup *page faults* are not tracked by any issue. The deterministic series cannot see them,
and the wall-clock series cannot separate them from noise.

Possible levers: build less of the registry eagerly; group start-up code with linker
ordering (`-Z` function-sections ordering or BOLT). [pgo-experiment.md](pgo-experiment.md)
measured PGO on a hot loop, not on startup layout, and does not cover this.

**Update (#19197, first slice): start-up code is now grouped.** `build.rs` passes
`linker/startup-text.ld`, an lld `INSERT BEFORE .text` script, to the link of the `mutsu` binary.
The script lists the ~1,700 functions start-up executes as hash-free section patterns, and
`scripts/gen-startup-layout.py` regenerates it from callgrind traces. On a CI-style release build
(non-incremental), `bench-startup` under `MUTSU_JIT=off` went from ~1,396 to ~1,217 minor
faults, and the resident text went from 27.4 MB to 8.8 MB. Over 2x40 interleaved runs, median
wall time fell 5-7%. The rest of the faults are not code. Here is where they go, measured with
`/proc/<pid>/stat` at `main` and at `exit_group` under gdb:

| phase | minor faults | main cause |
|---|---:|---|
| `execve` | ~143 | the kernel loading the binary |
| dynamic loader, before `main` | ~470 | ~80k `R_X86_64_RELATIVE` relocations dirty ~396 pages of `.data.rel.ro` (one copy-on-write fault each) |
| `main` to exit | ~555 | the interpreter thread's malloc arena (~1 MB anonymous memory, one fault per page), `.rodata`, `.rela.dyn` and the remaining text |

The two large items left both come from data, not layout. One is the relocations: a non-PIE
link would remove them, but it lowers ASLR and needs the maintainer's approval (see AGENTS.md
and #11158). The other is heap pages first touched during start-up.

**Update (#19197, second slice): one heap item is gone.** `build_builtin_registry` used to build a
scratch class table of ~570 KB: 1,024 inline buckets of ~530-byte `ClassDef`s. It now inserts
each class straight into the registry's sharded table. Faults went from ~1,225 to ~1,107, and
median wall time fell 5-8%. At the new heap peak (massif, `bench-startup`), these items remain:

| allocation | bytes | faulted in? |
|---|---:|---|
| two crash-report signal stacks | 512 KB | no: `mmap`ed and never touched |
| the registry's `ClassDef`s (~450 × ~530 B) | 238 KB | yes |
| ~1,000 small allocation sites | ~325 KB | yes |
| hash-table buckets of other registry tables | ~220 KB | yes |
| `Symbol`'s id → string chunk (4,096 × 24 B) | 98 KB | yes |

`.data.rel.ro` is 1.45 MB, and its relocations are why its pages are copied. Named symbols
cover 724 KB of it, of which 572 KB is anonymous constants. The unnamed rest is mostly vtables.
No single table dominates.

## Cross-cutting cause 2: instruction speed

Instructions, whole run, both interpreters under callgrind:

| benchmark | mutsu | rakupp | mutsu user time | rakupp user time |
|---|---:|---:|---:|---:|
| bench-ctor | 1.23G | 1.26G | 234 ms | 167 ms |
| word-count | 846M | 786M | 95 ms | 88 ms |
| bench-regex-match | 969M | 1,138M | 123 ms | 110 ms |
| bench-regex-split-subst | 1,323M | 1,565M | 209 ms | 139 ms |
| bench-yaml-parse-big | 876M | 489M | 130 ms | 83 ms |
| bench-ripemd-rounds | 602M | 423M | 61 ms | 46 ms |

cachegrind, same runs:

| | regex-match mutsu | regex-match rakupp | split-subst mutsu | split-subst rakupp |
|---|---:|---:|---:|---:|
| I1 misses | 41.1M | 22.5M | 7.7M | 0.06M |
| D1 misses | 4.3M | 1.8M | 8.2M | 4.6M |
| LL data misses | 159k | 15k | 591k | 568k |
| conditional mispredicts | 7.4M | 5.5M | 4.4M | 1.5M |
| indirect mispredicts | 4.1M | 2.9M | 0.3M | 4.5M |

On the first four rows mutsu loses at equal instruction counts. The I-cache and D-cache miss
counts and the mispredicts are where the time goes. This is the instruction-speed half of
#15737 (measured there on AttrX::Mooish: ~1.6 Ir/ns, 2.8% I1 miss rate, 13% branch mispredicts),
now seen on the plain benchmark rows as well. #8819 (instructions per opcode) is the other half
and matters on the rows where mutsu also runs more instructions (yaml, ripemd).

## Cross-cutting cause 3: malloc

glibc's `_int_malloc`, `_int_free`, `malloc`, `free` and `malloc_consolidate` are the top
exclusive symbols on every one of these rows (about 10-20% of instructions; ~25% on
split-subst). Preloading jemalloc (`LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.2`)
gave these medians of 15 alternating runs:

| benchmark | glibc | jemalloc | change | RSS |
|---|---:|---:|---:|---|
| bench-regex-split-subst | 273.7 ms | 231.6 ms | -15.4% | 67.6 → 72.5 MB |
| bench-yaml-parse-big | 162.3 ms | 143.4 ms | -11.7% | 56.3 → 59.0 MB |
| word-count | 116.4 ms | 104.3 ms | -10.4% | 39.0 → 41.5 MB |
| bench-regex-match | 159.8 ms | 150.1 ms | -6.1% | 48.0 → 50.7 MB |
| bench-ctor | 286.1 ms | 271.7 ms | -5.0% | 46.9 → 50.0 MB |
| bench-ripemd-rounds | 75.1 ms | 74.0 ms | -1.3% | 52.2 → 55.5 MB |
| bench-startup | 8.7 ms | 9.5 ms | +10% | 31.9 → 35.1 MB |

This differs from the negative result recorded in #15235. There, mimalloc as the binary's
`#[global_allocator]` gave +1.6% instructions, flat wall time and +59% RSS on ML::TriesWithFrequencies.
The two experiments differ in three ways: a different allocator, a different way of installing it
(a preload replaces glibc malloc for every caller, including C code; `#[global_allocator]` replaces
it only for Rust code), and a different workload. The rows above allocate many short-lived small
blocks, and on them the gain held over 15 alternating runs. An allocator change should be judged
on these rows *and* on #15235's workload. Note that `src/lib.rs` already installs a counting
`#[global_allocator]` for `alloc_stats`, which a real allocator would have to wrap.

The same preload measured a day later, after #19201 had cut the allocation count, splits in two.
Rows dominated by startup now get 7-11% *slower*, and the allocation-heavy rows gain about half
of what they did. See [Re-measurement](#re-measurement-2026-11-26-main-5f44cb0d).

## Row-specific costs

### bench-grammar-parse, bench-grammar-parse-deep

A ~150-250 character document, parsed once or three times. Whole-run instructions:

| | mutsu | rakupp |
|---|---:|---:|
| bench-grammar-parse-deep | 25.8M | 15.5M |
| bench-grammar-parse | 24.4M | 18.7M |

mutsu's -deep run breaks down as: interpreter construction 7.4M, parsing the program 5.1M,
`grammar_parse` 5.0M (of which the LTM NFA build `NfaBuilder::build_proto` is 1.6M), and
dynamic loading 1.5M. Everything except the match itself is one-time work. A 156-character
input cannot amortise it, so these rows measure startup plus first-parse cost. Covered by
#17640 (first-parse rule analysis), #10952 (closed; it already noted that startup counts double
on this row), and cause 1 above.

### bench-yaml-parse-big

- `use YAMLish` is ~27 ms on mutsu with a warm precomp cache, against ~10 ms on rakupp
  (`use lib` of the vendored YAMLish and MIME-Base64, nothing else).
- The timed section is ~1.3x rakupp (CI: 30.2 ms against 20.0 ms).
- About 40% of the parse is malloc. The action methods are ordinary Raku method calls.

Covered by #12917 (this exact row, "~5x rakudo and Raku++", architecture-level) and #7576.
Module load: #12026, #18012.

### bench-ripemd-rounds

`use Test` costs ~12 ms on mutsu even with a warm precomp cache. rakupp's Test is built in. The
timed section itself is *faster* on mutsu (45 ms against 50 ms locally). Covered by #11756 and
#12026 (load cost) and #7565 (the tax `use Test` puts on hot loops).

### bench-ctor

Same instruction count as rakupp (1.23G against 1.26G), so this row is causes 2 and 3. Inside
mutsu, `dispatch_bless` is 48% of instructions: construction-phase steps 36%, mostly the two
TWEAKs. `release_nested` (dropping nested values) is 11%. Covered by #9291 and #18356
(attribute access and slots in method bodies) and #7549 (hash key allocation).

### Regex rows (match, capture, split-subst, assertion)

Timed sections, local minimum of 5 runs: regex-match 49 ms against rakupp's 57 ms (mutsu
faster), regex-assertion 30 against 27, regex-capture 69 against 55, regex-split-subst 66 against
46. The whole-script gaps are startup plus causes 2 and 3. split-subst's RSS is 70 MB against
34 MB. It builds a `Vec<char>` of each subject once per call (the #8247 design, ~5% of
instructions), and the result assembly makes ~1.15M small deallocations. No open issue covers
the cache and allocation profile of these rows.

### word-count

Within noise: 846M against 786M instructions, 95 ms against 88 ms user time. The CI gap (1.01)
is startup.

## Existing issues

| part of the gap | rows | issue(s) |
|---|---|---|
| JSON::Fast compared with a native parser | json-fast* | dashboard note #19195; real target is #8673 (vs rakudo) |
| Instructions per opcode | yaml, ripemd, all | #8819 |
| Instruction speed (I-cache, branches) | ctor, regex*, word-count | #15737; #19199 for the regex rows |
| Startup instructions | all | #17504 (open), #18956 (fixed) |
| Startup page faults | all | #19197 |
| `use Test` load and tax | ripemd | #11756, #12026, #7565 |
| Module load re-runs BEGIN and traits | yaml (load) | #18012 |
| Grammar first-parse analysis | grammar-parse* | #17640 |
| YAMLish parse and actions | yaml | #12917, #7576 |
| Attribute access and slots | ctor | #9291, #18356 |
| Hash-key allocation | ctor, word-count | #7549 |
| Allocator choice | all | #19198; #15235 records one negative mimalloc result |

## Filed from this survey

These three had no issue when the survey was written. Each issue carries its own close
condition.

1. **Startup page faults**: #19197. ~28 MB of text touched by `Interpreter::new`, ~3 ms of
   system time per process. Goal: `bench-startup` at or under 1.10x rakupp.
2. **Global allocator on the allocation-heavy rows**: #19198. Re-run the jemalloc preload above
   as a real `#[global_allocator]` (jemalloc and mimalloc), keeping the counting wrapper, on
   these rows and on #15235's workload. Decide on wall time and RSS together.
3. **The cache and mispredict profile of the regex rows**: #19199, a sub-issue of #15737.
   Goal: `bench-regex-capture@section` at or under 1.00x rakupp.

## Re-measurement (2026-11-26, `main` `5f44cb0d`)

What changed since `13320ba6`:

- **#19197 slices**: start-up text grouped by a linker script (#19203), the scratch built-in
  class table removed (#19204), and `%*ENV` built on its first lookup (#19225).
- **#19201**: `.split` pieces share the invocant's buffer (#19202, #19207), and so do `comb`,
  `lines`, `words`, `substr` and an ASCII subject's Match text (#19213). The same work
  stopped a `for … -> $x` loop from building a `String` key per iteration.

### bench-det (CI, deterministic)

`bench-det-history.tsv`, interpreter series, `f03eae22` (2026-11-25) → `5815153c` (2026-11-26):

| benchmark | instructions | allocations |
|---|---:|---:|
| bench-startup | 7.51M → 5.38M (-28%) | 9,899 → 6,682 (-32%) |
| string-concat | -8.8% | -30% |
| bench-grammar-parse | -12% | -16% |
| bench-grammar-parse-deep | -7.6% | -13% |
| word-count | -14% | 768,041 → 339,829 (-56%) |
| bench-regex-split-subst | -10% | -26% |
| regex-match, regex-capture, ctor, yaml-parse-big, ripemd-rounds, json-fast-spdx | within ±2% | within ±3% |

The wall-clock series lagged: its newest row was `afd0efb7`, which predates #19213 and #19225.

### Local wall clock, page faults and the jemalloc preload

Release build of `5f44cb0d` and rakupp v5.0.1, `MUTSU_JIT=off`. Each figure is the median of 11
interleaved runs, timed as user + sys from `wait4`, on the noisy 4-core container (read every
ratio as ±10%).

| benchmark | mutsu | rakupp | vs rakupp | minor faults (mutsu / rakupp) | jemalloc preload |
|---|---:|---:|---:|---:|---:|
| empty program | 6.0 ms | 4.1 ms | 1.44 | 1,000 / 469 | +7% |
| bench-startup | 5.8 ms | 4.4 ms | 1.30 | 1,033 / 512 | +11% |
| string-concat | 7.8 ms | 6.5 ms | 1.19 | 1,226 / 567 | +11% |
| bench-grammar-parse | 11.3 ms | 7.1 ms | 1.59 | 1,667 / 761 | +3% |
| bench-grammar-parse-deep | 13.6 ms | 7.1 ms | 1.92 | 1,727 / 753 | +11% |
| word-count | 100.7 ms | 101.5 ms | **0.99** | | -6% |
| bench-regex-split-subst | 221 ms | 162 ms | 1.36 | | -8% |
| bench-regex-match | 157 ms | 124 ms | 1.27 | | -5% |
| bench-regex-capture | 241 ms | 171 ms | 1.41 | | -6% |
| bench-ctor | 290 ms | 188 ms | 1.54 | | -14% |
| bench-yaml-parse-big | 153 ms | 106 ms | 1.45 | | -6% |
| bench-ripemd-rounds | 80 ms | 58 ms | 1.38 | | -9% |

Measured this way, rakupp takes ~470 faults for an empty program, not the ~300 quoted in
cause 1. mutsu's count fell from ~1,200-1,400 to ~1,000, so the gap is now about 2x rather than
4x.

What this says:

- **Startup (#19197)** is still ~1.3-1.4x rakupp, and mutsu still takes twice the faults. The
  breakdown under cause 1 names what is left: ~400 copy-on-write faults from the PIE's
  relocations, which only a non-PIE link removes (#11158, needs the maintainer), and heap pages
  first touched during start-up (ADR-19206 targets the built-in registry).
- **How much of startup is the PIE?** The same source (`ef5fd793`, no `src/` change since
  `5f44cb0d`) linked as a non-PIE executable (`cargo rustc --release --bin mutsu -- -C
  relocation-model=static`: `EXEC`, no relative relocations) against the normal PIE build. Each
  figure is the median of 21 interleaved runs:

  | benchmark | PIE | non-PIE | rakupp | vs rakupp, PIE → non-PIE |
  |---|---:|---:|---:|---|
  | empty program | 6.13 ms, 1,086 faults | 5.22 ms, 697 | 4.74 ms, 534 | 1.29 → 1.10 |
  | bench-startup | 7.08 ms, 1,123 | 5.48 ms, 727 | 4.96 ms, 576 | 1.43 → 1.10 |
  | string-concat | 9.69 ms, 1,340 | 8.54 ms, 918 | 7.57 ms, 633 | 1.28 → 1.13 |
  | bench-grammar-parse | 16.27 ms | 14.98 ms | 9.80 ms | 1.66 → 1.53 |
  | bench-grammar-parse-deep | 13.65 ms | 12.80 ms | 6.96 ms | 1.96 → 1.84 |

  On the startup-bound rows the link mode is about three quarters of the gap: non-PIE leaves
  mutsu ~1.1x rakupp. The grammar rows barely move, because their gap is first-parse work
  (#17640), not startup. The dashboard's **ratio vs rakupp** cards for `bench-startup` and
  `string-concat` now say this. Whether to link non-PIE (giving up ASLR for the binary's own
  code) is the maintainer's decision, tracked in #11158.
- **word-count** has drawn level with rakupp. The split and for-loop allocation work removed most
  of its gap.
- **The jemalloc preload no longer helps across the board (#19198).** It now costs startup-bound
  rows 7-11%, and its gain on the allocation-heavy rows has roughly halved: word-count -10% →
  -6%, split-subst -15% → -8%. ctor's -14% (previously -5%) is within this box's noise. These are
  still preloads. #19198's real test is a `#[global_allocator]` build, which has not been made.
- **What remains on the heavier rows does not move with these changes.** ctor (1.5x),
  yaml-parse-big, regex-capture and ripemd-rounds (~1.4x) have flat instruction counts, so they
  sit with instruction speed (#19199, #15737) and per-opcode cost (#8819).

## Reproducing

```bash
# rakupp, the release this survey measured (bench.yml now pins v5.9.0)
curl -fsSL -o r.tgz https://github.com/ash/rakupp/releases/download/v5.0.1/rakupp-linux-x86_64.tar.gz
tar xzf r.tgz   # -> rakupp/bin/rakupp

# instructions (both are single-process, so the totals compare directly)
MUTSU_JIT=off valgrind --tool=callgrind target/profiling/mutsu benchmarks/<b>.raku
valgrind --tool=callgrind rakupp/bin/rakupp benchmarks/<b>.raku

# cache / branch simulation
valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes <binary> benchmarks/<b>.raku

# allocator A/B
LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.2 target/release/mutsu benchmarks/<b>.raku

# startup page faults: run each binary under os.wait4() and read ru_minflt / ru_stime;
# resident text: run `sleep 1` and read /proc/<pid>/smaps
```
