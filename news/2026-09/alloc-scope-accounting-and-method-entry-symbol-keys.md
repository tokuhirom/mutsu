# Per-scope allocation accounting, and the method-entry env keys it found

ADR-0019 G3 (`todo/perf/adr0019-g3-diffuse-bless-allocation-cost.md`) had been stuck on a
measurement problem, not a coding one. `bench-ctor`'s regression is *diffuse*: a flat `perf` profile
shows ~7% in `malloc`/`free` and ~6.5% in NaN-box GC/refcount ops with no single hot function, and
the obvious follow-up question — *which caller is doing all that allocating?* — could not be
answered, because call-graph attribution was unusable in the dev container (`--call-graph dwarf`
died on a stale `/root/.debug` build-id store, `--call-graph fp` produced garbage stacks through the
optimized build). The ticket's own next-steps list proposed building a counting allocator instead.
This is that tool, plus the first two fixes it found.

## `alloc_scope!` — exact, deterministic allocation counts per region

`src/alloc_stats.rs` adds a measurement-only cargo feature, `alloc-stats`. With it on, a counting
`#[global_allocator]` wraps `System` and `alloc_scope!("label")` opens an accounting region for the
rest of its block (`alloc_scope_named!` / `alloc_scope_end!` close one early, so a function can be
split into sequential phases without being re-indented into nested blocks). The report goes to
stderr at the end of the run:

```
cargo build --release --features alloc-stats
MUTSU_ALLOC_STATS=1 ./target/release/mutsu benchmarks/bench-ctor.raku
```

Each scope reports its entry count and its allocations/bytes both inclusive of nested scopes and
exclusive of them, so a nested set of scopes reads like a profiler's self-time column. Counts are
exact and do not depend on machine load, thermals, or binary layout — the same property that makes
the `MUTSU_VM_STATS` counters, rather than local wall-clock, the right thing to iterate against.

With the feature off — every ordinary build, and everything CI compiles — `alloc_scope!` expands to
nothing and the custom allocator is not installed, so there is no cost to leave the call sites in.
They are left in place along the construction path (`bless`, its attribute-default / named-arg /
BUILD / TWEAK phases, `run_construction_phase_steps`, and the compiled method-call entry split into
prologue / param-bind / env-setup / locals / body / epilogue), where they now serve as executable
documentation of that path's cost structure.

## What it found

Constructing one `bench-ctor` object (a 20-attribute class, `method new(*%_)` delegating to
`self.bless`, `TWEAK` at two MRO levels) cost 1,646,587 allocations over 5000 iterations. The
breakdown was not where the ticket had assumed:

- `bless` itself is cheap. Its own body, the attribute-default seeding loop, and building the
  instance came to ~21 allocations per construction combined — the `AttrMap::with_capacity`
  pre-sizing from the previous session's slice had already done its job.
- **64% of every allocation in the whole program was made inside `call_compiled_method`**, exclusive
  of everything it calls. Per method call that is ~70 allocations, of which ~29 are pure call-frame
  setup — paid identically by `submethod TWEAK(:$!spec) { }`, whose body is empty.

So the target was never `bless`. It was the per-method-call frame.

## Fix 1: the fixed per-call env keys are pre-interned symbols

Both compiled method-call paths opened a frame by writing a fixed set of env keys — `self`,
`__ANON_STATE__`, `?CLASS`, `?ROLE`, `_`, `!`, `__mutsu_callable_id`, `%_` — through
`Env::insert(String, Value)`, which allocates a `String` only to hand it to `Symbol::intern` and
drop it again. Bound parameters went the same way, with a `String` allocated per parameter per call.

`symbol::wk` already existed for exactly this problem: its `rebound_return` entry carries the note
that re-interning a name on a hot path "showed up as 5.3% of `bench-fib`", because the thread-local
intern cache is a string-keyed hash lookup. The method-entry key family had simply never been given
the same treatment. It has now — the fixed keys are `wk` symbols written with `insert_sym`, and
parameters intern their borrowed `&str` directly instead of allocating a copy of it first.

`insert_sym` deliberately does not run `env::note_env_key`, the latch that records whether any
`^placeholder` or `__mutsu_bound::`-family metadata key may exist. None of the fixed keys is one of
those, so skipping it is sound; a *parameter* name can be `^`-prefixed, so the parameter helper
calls `note_env_key` itself. A unit test pins each well-known symbol against its string spelling,
since a typo there would compile fine and silently write the wrong env key.

Measured: `mfast:env-setup` fell from 9.0 to 2.7 allocations per method call, and the whole program
from 1,646,587 to 1,536,588 allocations (-6.7%).

## Fix 2: the `BUILDALL`/`POPULATE` probe moves into the per-class plan

Every `bless` ended by asking whether the class declares a user `BUILDALL` or `POPULATE`, and asked
it by walking the MRO twice through `Registry::user_method_overloads` — interning both the class
name and the method name at every level, ~16 interns per construction, to reach the answer `None`
that essentially every class gives. That is pure class shape, so it now sits in `NativeCtorPlan` as
`user_buildall`, next to the `has_build` / `has_tweak` / `has_custom_bless` probes that were moved
there for the same reason. It allocated nothing, so the allocation count is unchanged; what it
removes is instruction count on a path `callgrind` showed spending 6.5% of the program in
`Symbol::intern`'s thread-local lookup.

## Result

Order-swapped min-of-9 A/B against the pre-change binary, `MUTSU_JIT=off`: `bench-ctor` -3.5%,
`bench-class` -4.6%. Both benchmarks improve in both orders. The change helps every method call in
every program, not just construction — `bench-class`, which has no `bless`-heavy shape at all, is
the larger of the two wins.

The ticket stays open: the tool has now localized the *next* target precisely (the implicit `*%_`
slurpy hash, built on every method call whether or not the body can observe it — 10.3 allocations
per call, ~10% of `bench-ctor`'s total), which is a compiler-analysis slice rather than a dispatch
one and is written up there as its own next step.
