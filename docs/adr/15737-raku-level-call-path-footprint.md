# ADR-15737: Shrink the Raku-level call path's instruction count and footprint

- **Status**: Accepted (user decision 2026-10-31, D1-D3 including the PGO step)
- **Issue**: [#15737](https://github.com/tokuhirom/mutsu/issues/15737) (carries #15688 AttrX::Mooish
  and #15498 Red `t/38-equal.rakutest`)
- **Input**: the perf-tuning skill (callgrind slope method, section 5a "slices of 1-2% cannot reach
  a multi-x goal"), the measurements below

## Context

After the individually measurable outliers were removed (inner `my sub` in methods #15700, `.^meta`
on a builtin HOW #15712, multi-method tiering #15722 / #15729, string return specs #15691, and the
earlier `.HOW`/`.WHAT`, `&f` call, attribute-default and short-name-index slices), two workloads are
still far from their goals and neither has a profile line above ~6%:

| Workload | mutsu | rakudo | goal |
| --- | --- | --- | --- |
| `Lazy.new; $i.foo` (AttrX::Mooish, #15688) | 4.4M Ir, 2.6-2.9 ms | 0.85M Ir, 0.158 ms | within 5x wall |
| Red `t/38-equal.rakutest` (#15498) | 191 s | 16 s | <= 120 s |

Measured on a profiling build of main@4071e9c1 (4 cores; Ir is the callgrind slope between two
iteration counts, wall is user time):

1. **Instruction count is 5.2x rakudo's** for the Mooish iteration (same method under valgrind).
2. **Instruction speed is ~3x worse than a plain call loop**: the Mooish iteration runs at ~1.6
   Ir/ns, a method-call loop at ~5 Ir/ns. Simulated cachegrind: I1 miss rate 2.8% (11M per 410M
   Ir), branch mispredict rate 13%, D1 miss rate 3%; sys time is ~0.5%.
3. **The call floor is not the problem**: a method call is 11.4k Ir / 2.3 us (rakudo ~1.9 us), a
   one-argument sub call 9.4k Ir / 1.65 us (rakudo 0.82 us).
4. **A method call is ~17 functions of 0.2-1.8k Ir each**, with no heavy one: per call 11.2k Ir =
   `call_compiled_method_fast_in_unit` ~6.2k inclusive (self ~1.75k; `pop_call_frame` ~0.9k,
   `try_enter` (JIT gate) ~0.55k, `reconcile_attrs` ~0.23k, `Env::scoped_child` ~0.2k,
   `Env::clone`, `method_attr_cells_for`, `method_body_package`, `push_method_routine_with_location`,
   ...) plus ~5k in the call site and resolution in front of it.
5. **Allocation is the largest cross-cutting term**: `malloc`/`free` is 15-30% of the Mooish and
   DEPARSE profiles. Before #15729 the largest allocation site was `MethodDef::clone` (several
   `String`/`Option<String>` fields: `role_origin`, `original_role`, `return_type`, ...).
6. **Build-level levers are small or unavailable**: thin LTO on the profiling profile gives 0-12%
   (inside run-to-run noise); fat LTO with `codegen-units=1` is OOM-killed on a 15 GB box (rustc at
   13.9 GB); there is no PGO/BOLT step in the release pipeline.

Section 5a applies: a 3-5x goal is not reached by shaving 1-2% terms one at a time. The two factors
(1) and (2) multiply, and both come from the same thing, a Raku-level operation that executes
through many small, cold Rust functions with per-call allocation.

## Decision (proposed)

Treat the Raku-level call path as one thing to make smaller, in this order, re-measuring #15688 and
#15498 after each step; stop at the first step that reaches a goal.

### D1. A per-call-site resolved plan for method and closure calls

Extend the idea behind #15235 / ADR-0113 (a call site resolved at compile time to its target) from
frame-lexical subs to the method-call fast path: the call site caches, keyed by the receiver's
class stamp and the registry stamp, the resolved `CompiledFunction`, its binding plan and its
frame shape, so the 17-function sequence in (4) collapses to: guard, bind, push frame, run, pop
frame. `reconcile_attrs`, `method_body_package`, `method_attr_cells_for` and the routine-frame
location are computed once per site, not per call. A guard miss falls back to the existing path
(no new slow path is added; the existing one stays as the guard-miss tail).

**Relation to the existing caches.** Resolution is already memoized per `(class, method)` in the
`caches` subsystem (`method_resolve_cache`, `fast_method_cache`, `last_method_resolve` in
`src/runtime/resolution_caches.rs`, invalidated by `method_cache_generation`). D1 does not add a
third resolution cache: the plan *extends* the `fast_method_cache` entry with what is derived
after resolution today (frame shape, attribute cells, body package, binding plan), and the
per-site part is only a slot that points at that entry so a hit skips the hash probe. The two
precedents it follows are the direct-mapped `call_ic` slots of the positional light sub-call
path (epoch-stamped, retired wholesale by bumping `pos_light_ic_epoch`) and the guard shape of
the built-in method-table lane (ADR-11276 §2.4/§2.5, `src/vm/vm_method_site_lane.rs`): a list of
cheap conditions, any of which failing leaves the call to the full path untouched.

**Threads.** The plan lives in per-interpreter state (the `caches` subsystem, which
`fork_for_thread` starts empty in a spawned thread), not in the shared `CompiledCode`. A slot in
`CompiledCode` would be mutable state reachable from several Raku threads and would need atomics
or a lock to stay memory-safe under ADR-12814; per-interpreter slots need neither, at the price of
each thread warming its own.

Expected effect: the call floor 11.2k Ir toward ~5k; the Mooish iteration performs a few hundred
calls, so factor (1) improves by up to ~1.3x on its own. The larger benefit is footprint: fewer
distinct hot functions is the only code-level lever on factor (2).

### D2. Remove per-call allocation from the method-call and `new` paths

Survey (`cg-summary.py --allocs`, A/B) and remove the allocation sites on a plain method call and
on `Foo.new`: `MethodDef` string fields to `Arc<str>` (clone becomes a refcount bump), call-frame
`Vec`s reused from a pool owned by the `frame` subsystem (ADR-10779; no new `Interpreter` field), `Symbol::intern` calls on names that are known at the
call site (3.7k interns per Mooish iteration; ~21% come from `user_method_overloads` and the
deferral resolution, which intern the same two names per lookup). Each item is a mechanical,
measurable change with its own test; together they address the largest cross-cutting term in (5).

### D3. Code layout (PGO) as a release-pipeline decision

Factor (2) is the I1/branch-miss term; the only lever is layout. A PGO step (instrumented run of
`make test`-style workloads, then `-C profile-use`) in the release workflow, or a
post-link BOLT step. This changes how every shipped binary is built, so it was put to the
maintainer as a decision rather than slipped into a workflow; the maintainer approved it on
2026-10-31. The constraints it lands under:

- **All four release targets stay required** (Linux/macOS x x64/arm64; `cut-release`). The
  instrumented binary has to run on the target architecture, so each target needs a runner of
  its own architecture for the training run. A target whose training run cannot be made to work
  ships its ordinary (non-PGO) build; no target is dropped or weakened to get PGO on another.
- **No hardening flag changes.** PGO/BOLT must keep PIE, RELRO and the other protections of the
  current build (the hard rule on security protections; #11104 / #11158). A BOLT variant that
  needs non-PIE or relocations stripped is out unless approved separately.
- **Training workload**: a fixed, checked-in set (the slice-1 benchmark loops plus a subset of
  `t/`), not the full roast, so release time stays bounded; the choice is measured in slice 6.
- **It ships only on a measured gain** on the slice-1 numbers and on #15688 / #15498, quoted in
  its PR.

## Deletion list (what D1 removes when it lands)

- The per-call recomputation in `call_compiled_method_fast_in_unit` of values that are functions of
  (class stamp, method): `method_body_package`, `attr_default_classes`, `method_attr_cells_for`,
  `has_alias_meta`, `class_role_param_bindings` probes.
- The `try_enter` JIT gate probe on non-hot method bodies (decided once per compiled function).
- The duplicate `user_method_overloads` / `get_method_overloads_with_role_fallback` string interning
  on the resolution path (the `Symbol` pair is part of the cached plan).

## Slice plan

1. **Measure harness** (no interpreter code): extend `scripts/bench-det.sh` (which already
   reports Ir and allocations) with the cachegrind I1/mispredict numbers, and check in the Mooish
   loop and the plain-call loop as its inputs, so each later slice quotes the same three numbers
   (Ir/iteration slope, wall, I1/mispredict). Also measure the method-call path's inclusive share
   (`call_compiled_method_fast_in_unit` and its call site) on a reduced Red `38-equal` loop: the
   per-call breakdown in (4) is from Mooish only, and how far D1/D2 can move #15498 depends on that
   share.
2. **D2a** `MethodDef` string fields to `Arc<str>`; A/B on DEPARSE and Mooish.
3. **D2b** interned names known at the call site (`user_method_overloads`,
   `own_overloads_at_level`, `resolve_deferral_expansion` take `Symbol`s).
4. **D1a** site-cached plan for the zero- and one-argument plain method call (the `fm.raku`/`calls.raku`
   shape), guarded by class stamp + registry stamp; wraps, `find_method` intercepts and user
   `FALLBACK` keep the existing path.
5. **D1b** extend to `callsame`/`nextsame` chains and multi methods once D1a's guard is proven.
6. **D3** PGO in the release workflow under the constraints in D3 (approved 2026-10-31); it
   touches `.github/` and so also waits for the maintainer's CODEOWNERS review.

Each slice is a `Refs #15737` PR with its own before/after numbers; #15688 and #15498 are
re-measured and commented after slices 3, 4, 5 and 6.

## Consequences

- Positive: addresses both factors with mechanisms that cannot go flaky (a guarded cache with the
  existing path as the miss tail); each slice is independently measurable and revertible.
- Negative: D1 adds a cache whose invalidation must be right (class stamp + registry stamp are the
  existing precedents: `Registry::type_stamp`, `fn_resolve_gen`); a missed invalidation is a wrong
  dispatch, so D1a needs the multi-method, wrap and runtime-`add_method` tests already used by
  #15722 / #15700.
- Unknown: how much of factor (2) is code footprint versus data-dependent branching; D3 is the
  experiment that answers it and is the only step whose benefit is not predictable from the
  numbers above.
- Unknown: how much of Red's 191 s is the method-call path at all. The estimates above are from
  the Mooish iteration; slice 1 measures Red's share, and if it is small, #15498 needs its own
  analysis rather than more of this plan.
- Negative (D3): the release workflow gets slower (an instrumented build and a training run per
  target) and gains per-architecture runner requirements.

## Alternatives considered

- **Keep slicing outliers.** Rejected by the measurements: the last six slices moved Red by ~2% and
  Mooish by 27% and then by ~2%; the remaining inventory is each <= 6% of an iteration.
- **A native (Rust) implementation of `AttrX::Mooish` / Red pieces.** Banned (AGENTS.md hard rule
  and ADR-0096).
- **Fat LTO + `codegen-units=1`.** Not buildable on the 15 GB development boxes (OOM); thin LTO is
  inside noise.
- **A tracing/trace-compiled method call in the JIT.** A larger change than D1 for the same call
  shape; revisit only if D1a falls short of the call-floor estimate.

## Open questions

- Which training workload gives D3 its gain at a bounded release time (measured in slice 6)?
- D1's per-site slot: direct-mapped like `call_ic` (cheapest, can thrash on collisions) or keyed
  by site id in a small hash (no collisions, one more probe)? Both are per-interpreter (see
  *Threads* under D1); slice 4 measures both on the `calls.raku` shape before choosing.
