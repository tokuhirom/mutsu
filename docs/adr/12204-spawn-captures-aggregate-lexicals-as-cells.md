# ADR-12204: A spawned block captures its `@`/`%` lexicals as shared cells

- **Status**: Proposed
- Date: 2026-11-04
- Refines: ADR-0010 (alternative B, "share captured lexicals as cells", the stated destination)
  and ADR-0129 (retired lane bindings follow their live children)
- Related: ADR-0068 (struct guard on aliased containers), ADR-0109 (native `is rw` scalar
  parameter cells), ADR-14632 (store places), #15020, #12204

## Context

ADR-0010 scopes the cross-thread store to a spawn lineage but keeps it **keyed by bare
name**. A name has one entry per lineage, so two live bindings of one name (a recursive
frame, a callee's `my %pars` next to the caller's, two closures from one routine) cannot
both be represented. Everything since has been a mask that decides, per call path, which of
the two bindings the entry means:

| mask | what it patches |
| --- | --- |
| `thread_redeclared_vars` + `container_name_is_redeclared` | a `my` that re-declares a lane name |
| `mask_thread_redeclared_params` / `thread_param_shadow_vars` | a parameter that shares a lane name |
| `frame_remask_armed` + `ThreadParamMaskGuard` re-mask on return | a returning frame's own `@`/`%` locals (#15156) |

Each new call path needs its own guard. In one week the same defect, "a callee that declares
`my %pars` and spawns leaves its entry answering the caller's `%pars`", was fixed three times:
the general sub and method paths (#15156), the light sub path (#16345) and the fast method
path (#16395). A probe over every call kind (plain/multi/private methods, `BUILD`, multi and
proto subs, closures, `&`-variables, `map`/`sort` blocks, recursion through `start`/`await`,
`gather`) now matches `raku`, so the symptoms are covered. What is left is structural: ~105
mask sites in `runtime_shared_vars.rs`, `builtins_atomic_*`, `vm_env_helpers.rs`,
`vm_var_assign_local_get.rs`, `vm_data_push_ops.rs` and `vm_call_method_mut_ops.rs`, and a
class of path that has no guard yet.

The scalar half of ADR-0010 B already exists. `Interpreter::block_captured_scalars`
(`runtime_thread.rs`) names the scalars a spawned block captures through the closure machinery
(`CompiledCode::free_var_syms`, transitively through the closures it calls through a captured
variable), `box_captured_lexicals` boxes them into a shared `ContainerRef` cell, and
`clone_for_thread_for_block` keeps those names **off the lane**. For them no mask is needed:
parent and child hold one cell, and a second binding of the name is a different cell. The
aggregates are the half that is still on the lane, because the lane is what makes
`my @a; await start { @a.push(1) }; @a` work: the child's element stores reach the parent through
the `__mutsu_atomic_*` element lanes, and a whole-aggregate store (`@a = ...`) in the child
reaches it through the name-keyed entry.

Measured while writing this: hard-wiring `container_name_is_redeclared` to `false` (ADR-0039
§17 made `@`/`%` lexicals resolve by slot or upvalue) fails exactly three files under
`t/concurrency/thread-lock/` (`atomic-elem-redeclared-after-start.t`,
`thread-callee-array-does-not-clobber-caller.t`, `thread-uncaptured-container-lane.t`), so
that predicate and the atomic-lane masking guard only this name-keyed store.

Why aggregates were not simply included in the scalar capture: ADR-0001 and ADR-0010 record
that the **write analysis is incomplete**. A name written by a separately-registered
class/role method (`lexicals.type_body_written_lexicals`), through an rw-argument sink
(`cas`, `⚛`), or from inside a string `EVAL` is invisible to the static capture set, so
treating the analysis as authoritative trades a deterministic collision for silent lost
updates (the `S12-construction/roles-6e.t` regression).

## Decision

**A spawned block (`start`, `Promise.start`/`.in`/`.then`, `supply`/`whenever` bodies, the
hyper/race batch threads) captures each `@`/`%` lexical it references as a shared
`ContainerRef` cell, exactly as it already does for a plain scalar, and an aggregate captured
that way has no lane entry.** The migration is **per name**, never a flag day.

1. **The capture set is the references, not the writes.** A name is cell-captured when it is a
   free variable of the block or of a closure it reaches (the existing
   `block_captured_scalars` walk, which already follows closures called through captured
   variables). Nothing depends on knowing who writes it, which is what made the write
   analysis unsound. A cell that is never written is harmless.
2. **Opaque names stay on the lane.** A name in `type_body_written_lexicals`, any name in a
   block that contains a string `EVAL` or a symbolic lookup (`::($name)`, `OUTER::`/`CALLER::`
   on the spawned frame), and a `state`/`our` aggregate keep today's behaviour. They are the
   remainder the masks must keep covering, and the set shrinks as the analysis learns them.
3. **Representation.** The aggregate stays a `Hash`/`Array` value inside the cell, in the
   parent's slot and env and in the child's env copy. Element stores, `push`/`pop`/`splice`,
   slices and whole-aggregate assignment already resolve a store target through
   `place_root_mut` / `descend_container_ref`, which steps through a `ContainerRef`; the
   structural-change guard of ADR-0068 serialises concurrent shape changes, and a store that
   replaces the aggregate does it through the cell. The `__mutsu_atomic_arr::` /
   `__mutsu_atomic_hash::` element lanes are not created for a cell-captured name: the
   atomic element operations (`⚛`, `cas`) take the cell's lock instead.
4. **The masks retire with the names they guard.** When a name has no lane entry, none of
   `container_name_is_redeclared`, `thread_param_shadow_vars` or the `ThreadParamMaskGuard`
   re-mask applies to it. Each mask is deleted when no name can reach it (its predicate gets
   an assertion first), smallest first: `container_name_is_redeclared`, then the re-mask on
   return, then the parameter masks.
5. **Closure creation, not call, pays.** Cells are boxed at spawn (and at a `for`-loop
   binding that a spawn captures), the same place scalars are boxed today; a routine call
   gets no new work.

## Slices

Each slice is its own PR with the three known-red tests and the 148 thread/shared/atomic
`t/` files as the specification (a red one is a design error, not a flake).

- **S1 - oracle.** `MUTSU_SPAWN_CAPTURE_CHECK=1` reports, at every spawn, the aggregates the
  capture walk would cell-capture and, for each lane entry later read or written, whether the
  name was in that set. A name that is read or written on the lane but was not captured is an
  opaque shape the analysis must learn before S2 may cover it. No behaviour change.
- **S2 - cell-capture aggregates at spawn.** Extend `block_captured_scalars` /
  `box_captured_lexicals` to `@`/`%`, gated per name by the S1 oracle's clean set. Whole-aggregate
  and element stores through the cell; no lane entry for those names.
- **S3 - delete `container_name_is_redeclared`** (the three known-red tests go green by
  construction) and the atomic-lane masking guard for cell names.
- **S4 - retire the call-path masks** (`ThreadParamMaskGuard` re-mask, then
  `mask_thread_redeclared_params`), leaving the lane only for the opaque remainder.

## S2 progress (2026-11-04)

Step 1 landed: a `start`/`Promise.start`/`Promise.in` block (any spawn through
`clone_for_thread_for_block`) captures by cell every referenced `@`/`%` lexical
that the spawning frame already holds in a `ContainerRef` cell (the ADR-0039
decl-site cell) and that is not opaque (registered class/role method write,
attribute, dynamic, qualified or `__` compiler name): `Interpreter::aggregate_cell_captured`.
Those names are excluded from lane seeding. `MUTSU_SPAWN_CAPTURE_CHECK` reports a
`cell-captured-lane-write` for any lane write to such a name; over `t/concurrency/**`
the only one is `@received` in `io-socket-async-idle-reader-threads.t`, written by a
block-less spawn (step 2). Step 2 (partial): the scheduler cue,
channel-backed `tap`, socket accept and nested `whenever <Promise>` threads use
`clone_for_thread_for_block` with their callback as the capture source. Remaining:
supply drivers, `Proc::Async` readers and `.then` chains (no single callback),
then measuring thread benchmarks.

## Alternatives considered

- **Keep adding masks (status quo).** Rejected: the defect class is "a call path without a
  guard", it recurred three times in one week, and each guard is a second place that must agree
  with the lane about which binding a name means (ADR-0010's own argument against alternative A).
- **Frame-versioned lane keys** (a frame reads the lane only through the binding it
  published). Rejected: it adds a second keying scheme as large as the masks it replaces and
  still needs a per-path "which frame am I" decision.
- **Capture aggregates by value (snapshot).** Rejected on the repository's own measure
  (AGENTS.md, "Gain and risk"): correct only if no write is ever missed, which the write
  analysis cannot promise. A cell tracks later writes by construction.

## Consequences

- A re-declared or recursive binding of a captured aggregate is a different cell, so the
  `Red t/61` family and the callee-vs-caller `%pars` family hold by construction.
- A captured aggregate costs one cell hop per access in the child; measure on
  `bench-*` thread benchmarks before S2 lands (`perf-tuning` skill).
- The opaque remainder keeps the lane, so the masks stay until S4 can prove it empty; this
  ADR does not remove them early.

## Open questions

- `state` aggregates and `our` aggregates in a spawned frame: keep on the lane for now
  (rule 2) or give them cells keyed by their declaring routine.
- Whether `supply`/`whenever` bodies, which run on a different scheduler, share the S2 gate
  or follow one slice later.
