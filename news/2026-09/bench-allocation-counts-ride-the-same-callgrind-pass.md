# Allocation counts ride along in the callgrind pass the bench CI already runs

The deterministic bench series ([#8085](https://github.com/tokuhirom/mutsu/issues/8085)) exists
because the wall-clock one cannot see an ordinary regression: its commit-to-commit noise floor is
16-33%, so its 1.5x alarm sits inside its own p90. Simulated instruction counts reproduce to ~0.1%
instead, which is what makes a 2% alarm mean something — and it is how the `bench-array` and
`sort`-comparator regressions of this month were caught within a commit of landing.

Allocation counts belong in exactly the same slot. "Does this change do less work?" has two honest
deterministic answers, and mutsu's perf history is full of slices whose whole point was to stop
allocating — a rejected parse alternative that built its message anyway, `resolve_type_in_current_package`
formatting a name per execution, `PError::expected` turning 434 literals into `String`s. None of
them had a tracked number to land against. The per-region `alloc_scope!` counters and
`cg-summary.py --allocs` answer "where", but nothing recorded "how many, per benchmark, per commit".

The reason it was not recorded was a wrong assumption about the price. `scripts/bench-det.sh` had a
`BENCH_DET_ALLOCS=1` path that ran a **second** instrumented pass under `--tool=memcheck`, roughly
tripling the script's wall time, so it was off by default and CI never set it.

But callgrind records a **call count for every (caller, callee) edge**, so the number of heap
allocations was already sitting in the profile the script was writing to `/dev/null`. Reading it is
one awk pass over a file that already existed; the second tool run was never needed. It is now a
default column, `benchmark instructions allocations`, recorded per commit in
`bench-det-history.tsv` on the `bench-data` branch, shown in the bench job's step summary with its
own 2% warning annotation, and offered as an `allocations` metric button on the trend page.

## Two things had to be got right

**Which layer to count.** Not Rust's `__rust_alloc*` shims: they are ordinary functions that LLVM
may inline into their callers, so they would undercount silently and differently per codegen unit.
The libc entry points are dynamic symbols valgrind always resolves, and Rust's allocator lowers onto
them one-to-one — the profile shows the chain directly, `__rustc::__rust_alloc` 236,405 ->
`__rdl_alloc` 236,405 -> `malloc` 236,419 on `bench-hash`. Entry points that call each other must
not both be counted (glibc's `reallocarray` calls `realloc`), which rules out counting the internals
they share.

**Callgrind's name compression.** A function name is spelled out once as `<id> <name>` and referred
to by id afterwards, and `fn=` (the function being defined) and `cfn=` (a callee) share one
namespace. A first attempt fed the id->name map from `cfn=` lines only, which is enough for `malloc`
— normally first seen as a callee — and silently misses `calloc` and `realloc`, which are first seen
as `fn=`. The count came out 1,374 low on `bench-hash` and looked perfectly plausible: 236,419
against memcheck's 237,590, a 0.5% gap easily explained away as a difference between the two tools.
Checking it against memcheck rather than accepting a number that looked about right is what caught
it.

## How stable it is

Repeated runs of one binary:

| | run 1 | run 2 | run 3 | spread | Ir spread, same runs |
| --- | ---: | ---: | ---: | ---: | ---: |
| `bench-hash` | 237,793 | 237,793 | 237,793 | 0% | 0.012% |
| `bench-grammar-parse` | 38,862 | 38,858 | 38,861 | 0.010% | 0.016% |

A wider two-run sample agreed: eight of ten series within 0.01%, four of them at exactly zero. So it
is at least as reproducible as the instruction count, and exact on most benchmarks. Where a count
does move, the cause is the same per-process `HashMap` seeding that moves Ir, reaching the allocator
through a string or table whose growth depends on iteration order. It is also the more stable of the
two across toolchains — a rustc bump re-codegens every instruction and steps that whole series at
once, but it does not change how many times the program asks for memory, so an allocation regression
stays legible across exactly the event that makes the Ir series hard to read.

The tenth series in that sample is worth its own line, because it is a warning and not noise:
`bench-json-fast` measured 1,777,030 allocations and then 1,311,894, **-26%**, from nothing but the
module precompilation cache that the first run after a build has to populate. That is the same trap
the perf-tuning skill's §0 records as 650M instructions (34% of the run) on the same benchmark — and
the allocation column states it more loudly than Ir does. It does not distort the recorded series,
because the bench CI builds fresh on every run and is therefore cold on every run, consistently. It
does distort a local A/B that compares a first run against a later one, which is exactly what §0
already tells you not to do.

## What it is not

It counts calls, not bytes: the same number of larger blocks reads as unchanged. Bytes need
memcheck, which is what the memcheck pass is now for — `BENCH_DET_BYTES=1` adds a `bytes` column, at
roughly 3x the wall time, which is why CI does not set it. And it is whole-process, so it cannot say
where an allocation came from; that is still `cg-summary.py --allocs` and `alloc_scope!`. As with
instruction counts, fewer allocations is not automatically faster — the number localizes a change,
the wall-clock series says whether it mattered.

The `allocations` column is appended after `toolchain` rather than inserted next to `instructions`
so that no existing row of the history had to be rewritten: every pre-existing row keeps its five
fields and simply has no sixth, and the trend page hides the metric button until the history has one.

Closes [#8959](https://github.com/tokuhirom/mutsu/issues/8959).
