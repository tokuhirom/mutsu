# ADR-0019 G3: diffuse allocation/hashing cost in attribute-heavy construction (bench-ctor/bench-class)

## Background

ADR-0019 G3 asked for a direct A/B performance comparison between the 2026-07-31 commit
(`426b36cd1`) and `main` HEAD, rather than trusting the `bench-data` branch tsv trend. That
comparison, done by building both release binaries in separate worktrees and measuring with a
`scripts/bench-ci.sh`-style script (hyperfine was unavailable, no passwordless `apt` to install
it), confirmed the regression the tsv trend hinted at is real, order-swap-verified to control for a
~5-8% "second binary measured runs faster" system bias:

- `bench-ctor` +13.5% (swap-order re-check: +8.2%)
- `bench-class` +8.5%
- `debug-guard` +11.6%
- `time-parts` +12.1%
- `method-call` +0.4% (weak/ambiguous — likely noise)

## What got fixed

`time-parts`'s ~20% portion was cleanly bisected (about a dozen extra release builds, narrowing by
date/commit-count) to commit `0448be29a` (ADR-0022 Slice 5): every plain scalar `my`/`state`
vardecl unconditionally ran a `format!` allocation + `HashMap::remove` to clear a
`__mutsu_constant_var::<name>` marker, even though the marker only exists after a `constant` has
actually been declared. Fixed in PR #6575 (merged): gate the removal on a `bool` that latches once
any constant scalar is seen. Verified locally: `time-parts` ~11.5% faster post-fix, other
benchmarks unaffected.

Cross-checked the rest of the codebase for the same anti-pattern (unconditional `format!` +
env-marker scan on a common hot path): `env.rs` already has five existing `AtomicBool`/`bool`
"seen" gates for other marker families (`CLOSURE_META_KEY_SEEN`, `BOUND_KEY_SEEN`,
`BOUND_SLICE_KEY_SEEN`, `ELEM_INDEX_META_SEEN`, `env_type_constraint_seen` in
`runtime_var_meta.rs`) with near-identical doc comments about the exact same bug shape. The
`__mutsu_constant_var::` marker (added 2026-08-11) was the one recently-introduced gap; no other
live instance was found by manual inspection of `vm_method_dispatch.rs`'s two other per-attribute
`format!` loops (both already gated: one behind `frame_has_container_ref`, the other behind
`any_attr_defaults`) or `class_introspection.rs`'s `add_alias_attribute_metadata` (already
data-driven — zero iterations when the class has no sigilless attributes).

## What's still open: bench-ctor / bench-class

`bench-ctor` (`benchmarks/bench-ctor.raku`: a 20-attribute class, two MRO levels of `TWEAK`,
`bless`-heavy, 5000 iterations) and `bench-class` did **not** bisect to a single commit — direct
A/B against intermediate builds across the 7/31-8/17 range showed no isolated jump, only gradual
drift, unlike `time-parts`'s sharp single-commit step.

A `perf record`/`perf report` flat profile (`MUTSU_JIT` default-on, `--profile profiling` build,
`benchmarks/bench-ctor.raku` looped ~15s for ~15k samples) shows cost spread across:

- `malloc`/`_int_malloc`/`_int_free` (~7% combined)
- `mutsu::value::nanbox::{payload_op,gc_op,arc_op}` (NaN-boxed value GC/refcount ops, ~6.5% combined)
- `SipHasher::write`, `hashbrown::HashMap::insert` (~2%)
- `_dl_relocate_object` (dynamic linker — process-startup cost; each benchmark run is a fresh
  sub-second process, so a nontrivial fraction of samples land in process startup/parse, not the
  steady-state loop)
- `Symbol::intern`, `Env::{insert,get_sym,cow_mut}`

No single hot function dominates — this reads as the fundamental allocation/hashing/GC-refcount
cost of constructing a 20-attribute object with two levels of `TWEAK` dispatch, 5000 times, not a
missing-guard bug like the `time-parts` one.

## Tooling blocker

Call-graph attribution was not usable in this environment:

- `perf record --call-graph dwarf` + `perf report -g` hung/stalled on `addr2line` errors
  (`addr2line /root/.debug/.build-id/<hash>/elf: could not read first record`) resolving a stale
  or malformed separate-debug-info entry under `/root/.debug/.build-id/`. Investigating this
  further requires root (only `perf` itself is NOPASSWD-sudo in this container, not general file
  access under `/root/.debug/`).
- `perf record --call-graph fp` resolved fast but produced garbage/invalid addresses in the
  reconstructed stacks (frame-pointer chain is not reliably preserved through the `profiling`
  profile's optimized+debuginfo build), so the caller attribution could not be trusted.

Without a working call graph, it's not possible from this session to say *which* callers dominate
the malloc/GC/hash cost (e.g., is it the attribute-cell HashMap construction on every `bless`? The
TWEAK submethod dispatch chain? Symbol interning of 20 distinct attribute names per instance?).

## Next steps for a dedicated perf session

- Fix or work around the `perf --call-graph dwarf` addr2line issue (check/rebuild the local
  build-id debug cache, or try `perf record` with `--call-graph dwarf,<smaller-size>` /
  a newer perf, or run outside this container where the debug store isn't stale).
- Alternative to `perf` entirely: build a debug/instrumented binary with a counting
  `#[global_allocator]" wrapper (or reuse the existing `MUTSU_VM_STATS` counter mechanism if it can
  be extended) to count allocations per `bless`/`TWEAK` call directly — deterministic, environment-
  independent, and answers "how many allocations does constructing a 20-attr object cost" without
  needing symbolized call graphs at all.
- Re-run the same A/B methodology from this session (build 7/31 and HEAD, `MUTSU_JIT=off`,
  order-swapped) once a fix lands, to confirm the drift actually narrows.

## 2026-08-17 (later session): `AttrMap` pre-sizing

Picked up the "pre-size the map from the class's known attribute count" idea from the previous
session's next-steps list and landed it: `AttrMap::with_capacity` (new method) is now used at the
three construction sites that already know their final attribute count up front from a per-class
list —

- `dispatch_bless`'s default-attribute-value loop (`plan.class_attrs.len()`)
- `create_default_attr_slots` (the `CREATE` path, `collect_class_attributes(..).len()`)
- `build_native_default_instance` (the native default-ctor fast path, `class_attrs.len()`)

— avoiding `hashbrown`'s incremental `RawTable::reserve_rehash` growth (visible in the flat
profile at ~2% `hashbrown::HashMap::insert`/`SipHasher::write`) when the final size is already
known. `cargo build` + `cargo clippy -- -D warnings` clean; `t/class*.t t/bless*.t t/new*.t
t/attribute*.t t/role*.t t/mixin*.t t/create*.t` (998 tests) and the full `t/` suite (29792 tests)
pass.

Local A/B (worktree baseline at `main` HEAD `3a3a2713b` vs this change, `MUTSU_JIT=off`,
min-of-12/15 per side, order-swapped across three rounds) on a machine under variable background
load (uptime load average 6-13 on 12 cores during measurement):

- `bench-ctor` (20 attributes): consistently faster across all three rounds — roughly 10-20%
  (e.g. round 3: new 0.39s vs baseline 0.49s). This is the shape the fix targets.
- `bench-class` (3 attributes): no consistent direction, differences within noise — expected,
  since a 3-entry map barely pays incremental-growth cost either way.

This is a small, structurally-safe change (pre-sizing a `HashMap` never changes its contents), so
it was not gated on a clean local measurement — the project convention is that documented bench
numbers come from the bench CI trend (`bench-history.tsv`), not local runs, and this container's
load made a tight local confirmation unreliable regardless. Left this ticket open rather than
closing it: `env_deep_copies` (S2, `todo/tickets/bench-ctor-construction-parity.md`) and the
GC-candidate-push family are still diffuse cost, matching the "no single hot function dominates"
conclusion from the earlier profile. Next dedicated-session step is still the counting-allocator
build (previous section) if further attribution is wanted, or checking the bench-CI trend after
this PR merges to see how much of the 7/31-vs-HEAD drift narrows.

## 2026-08-17 (same session): `debug-guard`'s real cause found — a per-name, not per-program, gate

The original A/B list at the top of this ticket also names `debug-guard +11.6%`, left uninvestigated
alongside `bench-ctor`/`bench-class`. `benchmarks/debug-guard.raku` has nothing to do with
construction — it's `constant DEBUG = False;` followed by a hot loop calling a `sub` with a plain
`my $y = ...`. That shape is a red flag for the *same* commit (`0448be29a`, ADR-0022 Slice 5) whose
`time-parts` bug was already fixed this session, and turned out to be a second, unfixed bug in the
same mechanism.

The #6575 fix gated the `__mutsu_constant_var::` marker-removal `format!` + `env.remove()` on a
single **program-wide** `bool` (`any_constant_var_marker_set`): once *any* `constant` scalar was
ever declared anywhere in the program, every subsequent ordinary `my`/`state` scalar declaration —
any name, anywhere, for the rest of the run — paid the removal cost, because the bool has no way to
say "but not for *this* name." `debug-guard.raku` declares exactly one constant (`DEBUG`) at the
top, which immediately flips that bool permanently — so the hot loop's 1,000,000 `my $y = ...`
declarations (unrelated name, unrelated scope) all still pay full `format!`-allocate-a-string +
`HashMap::remove` cost, identical to before #6575's fix for a program with *zero* constants. The
fix only ever helped the "no `constant` anywhere in the whole program" case, which `time-parts`
happened to be but `debug-guard` (and any real program mixing `constant` with hot-loop `my`s) is
not.

**Fix:** replaced the single `bool` with `constant_var_names_seen: FxHashSet<String>` (same field,
`src/runtime/mod.rs`), populated only on an actual `constant` declaration (rare) and consulted by
*name* on every `my`/`state` vardecl (`src/vm/vm_var_assign_set_local.rs`) — `my $y` in a program
that only ever declared `constant DEBUG` now does a cheap hash-set miss on `"y"` instead of an
allocate + remove on `"y"`'s marker key. Thread-clone init (`runtime_thread.rs`) keeps the same
"start empty" semantics the old bool had (unrelated pre-existing behavior, not touched).

Local A/B (same worktree-baseline methodology, `MUTSU_JIT=off`, min-of-8/10, order-swapped, using a
throwaway 10x-iteration variant of `debug-guard.raku` in `tmp/` for a clearer signal against
process-startup noise): consistently ~7-8% faster, both orders (round 1: new 1.12s / base 1.22s;
round 2 swapped: new 1.13s / base 1.21s). `t/*constant*.t` (18 files) and the full `t/` suite
(29792 tests) pass; `cargo clippy -- -D warnings` clean.

Landed as a second commit on the same PR as the `AttrMap` pre-sizing fix (both are G3-investigation
findings from this session; see the PR for the combined test plan).
