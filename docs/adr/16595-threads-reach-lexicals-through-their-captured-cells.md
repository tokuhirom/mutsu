# ADR-16595: Code on another thread reaches a lexical through its captured cell; the name-keyed lexical lane is retired

- **Status**: Accepted (2026-11-05, by tokuhirom)
- **Date**: 2026-11-05
- **Supersedes**: [ADR-12204](12204-spawn-captures-aggregate-lexicals-as-cells.md)
  (its goal stands, its mechanism and slice plan do not; see §5)
- **Completes**: ADR-0010 alternative B ("share captured lexicals as cells", the
  stated destination)
- **Related**: ADR-0039 (container lexicals resolve lexically; §8.6 transient lane,
  §17 "the masks belong to #12204"), ADR-0055 (closure free variables resolve to
  their own binding; the vouch and its complement), ADR-0068 (a cross-thread
  container write is synchronized at the container), ADR-0129 (retired lane
  bindings), ADR-12529 (frames resolve names through the static link),
  ADR-12814 (concurrency follows rakudo), ADR-14632 (element stores operate on
  the declaring slot)
- **Carrier issue**: #16595 (parent #12204)

## 1. Context

### 1.1 What the lane is, and what it does

A spawned thread (`start`, `Promise.start`/`.in`/`.then`, supply and `whenever`
drivers, socket/proc readers, the scheduler, hyper/race batches) runs on an
`Interpreter` built by `clone_for_thread_excluding`
(`src/runtime/runtime_thread.rs`). The child gets a **copy** of the parent's
`env`, and the seeding loop in that function publishes every live user lexical
into `threads.shared_vars`, a store keyed by **bare name** and scoped to the
spawn lineage (ADR-0010). Three different jobs are done by that one store:

| job | why the lane does it | where it belongs |
| --- | --- | --- |
| (a) publication: the child sees the parent's binding | the child runs on an env copy | the closure's capture (upvalue or cell) |
| (b) a replacing write (`@a = ...`, `$x = ...`) reaches the owner | a copy cannot carry a rebind back | a shared `ContainerRef` cell |
| (c) concurrent structural writes serialize (`__mutsu_atomic_arr::`/`__mutsu_atomic_hash::`) | predates ADR-0068 | the container's own lock (ADR-0068, landed) |

Container mutation is write-through-the-shared-node (ADR-0039 §2), so an env
copy already aliases the parent's `Array`/`Hash` node; and ADR-0068 already
serializes a structural change at the node or the cell. So (c) has a home
today, and (a)/(b) have one wherever the binding is a cell. The lane is only
*needed* for a name that some other thread reaches **without** holding a cell.

### 1.2 The cost of a name-keyed store

A bare name has one entry per lineage, so two live bindings of one name (a
recursive frame, a callee's `my @x` beside the caller's, two sibling workers'
`my @w`, an inner block's shadowing `my @d`) cannot both be represented. Every
mask since ADR-0010 decides, per call path, which binding the one entry means:
`thread_redeclared_vars` + `container_name_is_redeclared` (16 sites in 11
files), `mask_thread_redeclared_params` / `thread_param_shadow_vars`,
`frame_remask_armed` + `ThreadParamMaskGuard`, ADR-0039 §8.6's
`transient_lane_containers`, ADR-0129's retired-binding boxes. The same defect
("a callee's `my %pars` answers the caller's `%pars`") was fixed three times in
one week (#15156, #16345, #16395). Rakudo has none of this: a lexical lives in
its frame's lexpad, a closure captures the outer frame, and a thread is just a
closure invoked elsewhere.

### 1.3 What ADR-12204 tried, and what was measured

ADR-12204 migrated aggregates name by name to cells **at spawn time**, gated by
an oracle, and planned to delete the masks once enough names had moved. Its S2
landed (#16427, #16438, #16446, #16458): a spawn cell-captures the `@`/`%` its
block or callback references **if the spawning frame already holds them in a
cell**. Then two measurements contradicted its plan:

1. With S2 in place, hard-wiring `container_name_is_redeclared` to `false` still
   fails the same three files (`atomic-elem-redeclared-after-start.t` 12,
   `thread-callee-array-does-not-clobber-caller.t` 9,
   `thread-uncaptured-container-lane.t` 19). More capture sources do not help.
2. A spawn-time reachability filter (skip seeding any aggregate a block that
   calls no unseen code does not name) turns those three green but breaks
   `t/concurrency/supply/supply-block-nested-whenever-serialize.t` 3-4: the
   tap callback's `@log` was reached only because *some* spawn had published
   it (#16571's amendment).

The probe that settles both (2026-11-05, debug build, all 456
`t/concurrency/**` files, prototype patch with each change behind an
environment switch):

| config | change | result |
| --- | --- | --- |
| A | `container_name_is_redeclared` hard-wired `false` | 10 files red |
| **D** | **every `@`/`%` an escaping closure captures gets the declaration-site cell, vouched or not** | **456/456 green** |
| B | D + seed no celled aggregate and no aggregate the spawned block does not name + A | 7 red |
| C | B without D | 8 red |

Two facts follow.

- **D is safe and is the missing half of S2.** ADR-0055's vouch (`authoritative_free_vars`)
  exempts a captured container from the declaration-site cell when the owning
  frame provably does not write it. That proves single-thread *name resolution*;
  it says nothing about another thread, so every vouched container (the
  `@log` above: the owning frame only reads it) fell back to the lane. With
  the cell, the tap callback holds `@log`'s cell from its creation, on whatever
  thread it later runs, and no publication is needed.
- **B's residue is the lane's consumers, not its publishers.** The three files
  still fail under B because the lane is **written and read by name** on paths
  that never consult a capture: `cas(@d[0], ...)` compiles to
  `__mutsu_cas_array_elem("@d", @d, ...)` (`src/compiler/expr_call.rs` ~1792,
  ~2010, ~2029) and writes the outer `@d`'s lane entry from an inner block
  (`[3, 9]` instead of `[1, 1]`); a worker's `push` onto its **own**
  `my @shadowed` is routed through `__mutsu_atomic_arr::` whenever
  `is_thread_clone()` (`src/vm/vm_data_push_ops.rs` ~232) and lands on the
  parent's entry (`w` instead of `PARENT`); sibling workers' `my @w` collide the
  same way (`10,0|10|20`). The masks guard exactly these routes. They go when
  the routes do, not when seeding is filtered.

Under A, B and C the two `cro-*-lexical(s).t` files, `promise-then-sibling-ordering.t`
and `virtual-scheduler-await-ordering.t` also fail (the Cro ones exit 255 under
B/C); they fail under A alone, so they are what the redeclared predicate
still guards while the lane is live. They are part of slice 3's acceptance
(§3), not evidence against this decision. (The prototype read its switch with
`std::env::var_os` inside the hot predicate, so the two timeouts among them may
be partly that overhead; re-measure them without it.) A's other three
failures (`thread-callee-aggregate-keeps-caller-binding.t`,
`thread-callee-method-aggregate-keeps-caller-binding.t`,
`thread-closure-own-aggregate-binding.t`) pass under B: declaration-site cells
fix them.

Rakudo's observable semantics the end state must keep were pinned against
`raku` (all agree on today's mutsu): a dynamic read and written inside `start`
(`my $*E = 0; await start { $*E = 5 }; $*E` is `5`: one container), one
`state` per closure clone across threads, one lexpad per call under
concurrency, a replacing write through a closure made before the thread, a
nested named sub reached only through a call, a caller and callee `@x` live at
once, `EVAL` inside `start`, `.then` mutating its creator's lexical.

## 2. Decision

**A user lexical that code on another thread can reach is a shared
`ContainerRef` cell from its declaration, and every thread reaches it through
the capture of the code it runs. No user lexical is published into the
name-keyed `shared_vars` store, and no operation on a user lexical reads or
writes that store by name.** This is rakudo's model (a thread runs a closure
over its outer lexpad) restated for mutsu's cells.

1. **The cell is born at declaration, never at spawn.** A closure created
   before a thread exists (a `tap` callback, a `.then` block, a closure stored
   in a variable) captured whatever the binding was at its creation; boxing at
   spawn cannot reach it (ADR-12204 rule 5 is withdrawn). The trigger is the
   compiler's escaping-capture set: every own `@`/`%`/`$`/`&` lexical an
   escaping closure captures gets the declaration-site cell, **independent of
   the ADR-0055 vouch**. The vouch keeps its single-thread role (a vouched
   *read* may skip the by-name fallback) but no longer withholds a cell.
2. **Code that runs a frame's own bytecode on another thread counts as an
   escaping capture.** A hyper/race batch runs the enclosing frame's
   loop-body range, not a closure (`src/vm/vm_hyper_race_parallel.rs`): the
   compiler adds every own lexical that range references to the
   escaping-capture set, so it is celled by rule 1. No run-time free-variable
   pass.
3. **Atomic and structural operations address the place, not the name.**
   `cas`, `⚛`/`atomic-*`, and element/whole stores on a lexical compile to an
   operation on its slot or cell (ADR-14632's place), and take the container's
   or cell's lock (ADR-0068). The by-name helpers
   (`__mutsu_cas_array_elem("@a", ...)` and kin) and the
   `is_thread_clone()`-unconditional routing into `__mutsu_atomic_arr::` /
   `__mutsu_atomic_hash::` are deleted for user lexicals.
4. **The spawn seeds nothing user-lexical.** `clone_for_thread_excluding`'s
   seeding loop stops publishing plain lexicals of any sigil. With it go
   `container_name_is_redeclared`, `thread_redeclared_vars` for lexicals,
   ADR-0039 §8.6's transient lane, the parameter masks
   (`mask_thread_redeclared_params`, `thread_param_shadow_vars`,
   `ThreadParamMaskGuard`, `frame_remask_armed`), and ADR-0129's box for
   lexical names. No reachability analysis is needed: a name no capture holds
   is, by rule 1, a name no other thread's code can name.
5. **What is not a lexical keeps its own mechanism.** Attribute storage
   (`@!x`) is reached through the shared instance. A dynamic (`$*x`, `@*x`) is
   reached through the caller chain plus the spawn point (rakudo 6.d), as one
   container, which ADR-12529's dynamic link provides; until then dynamics
   keep today's path. `state` keeps its root-store cell
   (`__mutsu_shared_state::`, ADR-0010 "Internal keys"). `our` is a package
   symbol. Internal `__mutsu_*` keys are not lexicals and are unaffected.
6. **Opaque access is a view of the static chain, not a lane.** A lexical
   written by a separately registered class/role method
   (`lexicals.type_body_written_lexicals`), or reached by string `EVAL`,
   `::('$x')`, `OUTER::`/`CALLER::`, resolves through the declaring frame's
   static link (ADR-12529 §2.1/§2.3). Until that phase lands, these names, and
   only these, keep the lane; the set is closed, listed in one predicate, and
   shrinks to empty with ADR-12529.

**Memory safety vs. races.** As on rakudo (ADR-12814), a program racing on one
container without synchronization may observe torn logical state but never
memory unsafety: ADR-0068's guard on the node/cell is the only lock, and it is
taken by every structural write regardless of how the container was reached.
Removing the lane removes no lock that ADR-0068 relies on (its §4 "(B)"
synchronizes at the container precisely so that it does not depend on the
lane).

## 3. Migration

Each slice is its own PR, filed as a sub-issue of #16595. The specification is
the full `t/` (not only `t/concurrency/**`), the whitelisted roast, and the
bundled-library battery (`scripts/battery-testsuite.sh`; ADR-0039 §13: `t/` and
roast alone have been green for changes in this area that the battery
rejected). Run `scripts/dev gate --full` before publishing a slice.

### Slice 1 (#16596) - declaration-site cells for every escaping-captured lexical

- In `CompiledCode`'s free-variable computation (`src/opcode.rs`, where
  `needs_cell_unvouched_containers` is computed from `escaping_captured_own`
  and `vouched`, ~line 12020), drop the `!vouched.contains(sym)` filter for
  `@`/`%`. Rename the field to `needs_cell_captured_containers` and update its
  doc comment (it is no longer "the complement of the vouch").
- Keep `box_decl_local_container_cell`'s native-element refusal (`my int @a`)
  as a named exception with a `// TODO:` pointing at slice 5; such a container
  keeps today's path.
- The scalar half: `needs_cell_unvouched_locals` likewise stops excluding
  vouched names for **non-plain values only if** a measurement shows a
  thread-reached scalar on the lane after the change (the
  `MUTSU_SPAWN_CAPTURE_CHECK=1` oracle, `src/runtime/shared_store/capture_check.rs`,
  reports `uncaptured-lane-write`). Do not widen it speculatively.
- `aggregate_cell_captured` in `runtime_thread.rs` then covers every captured
  aggregate; no change needed there.
- Acceptance: the gate's full profile green; `MUTSU_SPAWN_CAPTURE_CHECK=1` over
  `t/concurrency/**` reports no `uncaptured-lane-write` for a plain lexical
  except the slice-2 (hyper/race) and slice-5 (opaque) groups; a benchmark
  comparison (`perf-tuning` skill) of the thread benchmarks and of a
  single-thread `.map({ @x[$_] })`-shaped loop, before/after, quoted in the PR.
  A measurable regression is a perf issue to file, not a reason to keep the
  vouch exemption (correctness first; AGENTS.md "Gain and risk").
- Pitfall from history: ADR-0039 §7.1d recorded broader declaration-site boxing
  regressing ~12 files through decont leaks, before ADR-0055 slice 1b made the
  unvouched subset safe. If that recurs, the fix is at the leaking read (it
  must deref the `ContainerRef`), never a re-exemption.

### Slice 2 (#16597) - hyper/race batch bodies are escaping captures

- In the compiler, when compiling a `hyper for` / `race for` body (and
  `.hyper`/`.race` loop forms that run a frame range), add every own lexical
  the body range references to the frame's escaping-capture set, so slice 1's
  rule cells it.
- Acceptance: a new `t/concurrency/` test where a `race for ^4 { ... }` body
  pushes to, element-assigns and whole-assigns an outer `my @r`/`%h`, with a
  same-named `my @r` in a routine the body calls, matching `raku`; the oracle
  reports nothing for the hyper/race group.

### Slice 3 (#16598) - atomic and structural operations address the place

- `cas` on an element (`src/compiler/expr_call.rs` ~1792/~2010/~2029): emit a
  place-addressed op (the slot or upvalue of the container, as ADR-14632 does
  for element stores) instead of `__mutsu_cas_array_elem("@a", @a, ...)`; the
  VM side (`src/vm/vm_call_atomic.rs`, `src/runtime/builtins_atomic_cas*.rs`)
  CASes under the container's ADR-0068 lock. Same for `⚛`, `atomic-fetch-*`,
  and `builtins_atomic_index.rs` / `builtins_atomic_elem_cell.rs`.
- `src/vm/vm_data_push_ops.rs` (~232), `vm_shared_array_lane.rs`,
  `vm_var_assign_element_fast.rs`, `builtins_celled_array.rs`: a plain user
  lexical never routes into `__mutsu_atomic_arr::`/`__mutsu_atomic_hash::`;
  the write goes to the slot/cell under ADR-0068's guard. Keep the lane path
  only for slice 5's opaque predicate.
- Every `// Cost:` line on a touched op is restated (AGENTS.md).
- Acceptance: with `container_name_is_redeclared` hard-wired to `false`, the
  three files of §1.3 and the four extra failures of config A
  (`cro-client-repeated-await-response-lexical.t`,
  `promise/await-after-cro-request-keeps-caller-lexicals.t`,
  `promise/promise-then-sibling-ordering.t`,
  `thread-lock/virtual-scheduler-await-ordering.t`) pass. Do not land the
  hard-wiring in this slice; it is slice 4's deletion. The contention tests
  ADR-0039 §8.6 names (`t/concurrency/concurrent-array-index-assign.t`,
  `t/concurrency/concurrent-hash-assign.t`, `t/routines/dispatch/cas-multidim-cells.t`) are in the
  acceptance set; run each 5x on a release build.

### Slice 4 (#16599) - the spawn seeds nothing user-lexical; delete the masks

Status: landed. Measured findings: a scalar holding a Seq/Slip needed the
declaration cell (the `box_captured_lexicals` shape refusal was lifted for
them); Channel/Promise/Lock scalars are shared by the env clone itself; the
opaque remainder is still seeded as described here.

- `clone_for_thread_excluding`: skip every plain user lexical (all sigils) in
  the seeding loop except names in slice 5's opaque predicate. Delete
  `block_captured_scalars`' exclusion bookkeeping that only served the
  seeding, `block_referenced_containers`, `transient_lane_containers` and its
  drain in `sync_shared_vars_to_env`.
- Delete, smallest first and each behind an assertion for one green CI run if
  in doubt: `container_name_is_redeclared` (16 sites), the lexical use of
  `thread_redeclared_vars`, `thread_decl_in_flight`, the parameter masks
  (`mask_thread_redeclared_params`, `thread_param_shadow_vars`,
  `ThreadParamMaskGuard`, `frame_remask_armed`), and ADR-0129's retired box
  for lexical names. Their tests stay as regression pins (they describe
  behaviour, not mechanism); delete only tests of the internal mechanism.
- `make check-interp-fields`: removed `ThreadState` fields drop out of
  `scripts/interp-fields.d/`.
- Acceptance: full gate, roast, battery; `t/concurrency/**` green with no
  masks; the `MUTSU_VM_STATS` spawn-seeding counters show zero user-lexical
  inserts on `t/concurrency/**`.

### Slice 5 (#16600) - the opaque remainder, through the static link

- Depends on ADR-12529 phase 3 (a frame's name view built from the static
  chain). Route `type_body_written_lexicals` names, string `EVAL`, symbolic
  lookup and pseudo-package access through that view; dynamics through the
  dynamic link with the spawn point as the root of a thread's caller chain.
- Then `shared_vars` holds no user lexical at all; what remains is internal
  `__mutsu_*` keys. Whether the lineage store (ADR-0010) is still needed for
  those is decided then, not here.
- Native-element containers (`my int @a`) get their cell or a documented
  equivalent here.

### Order and parallelism

Slices 1 and 2 are independent. Slice 3 needs neither, but its acceptance (the
red files pass with the predicate off) needs slice 1. Slice 4 needs 1-3.
Slice 5 needs 4 and ADR-12529 phase 3. Slices 1-3 can run in parallel
worktrees; they touch disjoint files except `src/opcode.rs` (1, 2).

## 4. Consequences

- The defect class "a call path without a mask" disappears by construction: a
  second binding of a name is a second cell, and no code looks a lexical up by
  name across threads.
- Every escaping-captured container costs one `ContainerRef` hop per access in
  its owning frame, also in single-threaded programs (the vouched ones paid
  nothing before). Slice 1 measures it; the cost contract is one hop, O(1),
  with no dependence on frame size, lineage depth or thread count, which is
  better than today's lane lookup on a worker.
- A spawn no longer walks the parent env to seed it: `clone_for_thread`'s
  cost stops scaling with the number of live lexicals (the seeding walk's
  `record_spawn_seeding` counters measure it today).
- Unsynchronized concurrent writes to one container behave as on rakudo
  (memory-safe, logically racy). Today's lane silently serialized some of
  them; a test that relied on that was relying on non-rakudo behaviour and is
  fixed by adding the lock rakudo would need, after checking it under `raku`.
- ADR-0039 §17's statement that the masks go "when the store is keyed by
  binding identity" is fulfilled differently: there is no store to key.

## 5. Alternatives rejected

- **Continue ADR-12204 (spawn-time capture per name, then a reachability
  filter).** Rejected on measurement (§1.3): spawn time is too late for
  closures made before the thread, and the filter must answer "which names can
  any live callback reach", which only cells at declaration answer.
- **Key the lane by binding identity (frame-versioned keys, or the store's
  `Gc` node).** Rejected by ADR-12204 and ADR-0039 §8.6 (2): a second keying
  scheme as large as the masks, and node identity is destroyed by `make_mut`
  under contention.
- **Keep the vouch exemption and cell only containers that a thread-escaping
  closure captures.** Rejected: thread escape is not decidable at compile time
  (a `tap` callback is an ordinary closure argument); the probe shows the
  unconditional rule is green.
- **Snapshot captured aggregates by value.** Rejected as in ADR-12204: correct
  only if no write is missed.

## 6. Open questions

- Whether slice 1's single-thread cost needs a mitigation (e.g. an upvalue
  that caches the cell's payload for a read-only, non-escaping-after-spawn
  window). Decide from slice 1's numbers; do not pre-build it.
- Whether the ADR-0010 lineage store survives slice 5 for internal keys only,
  or those keys move to their own typed homes (`state` cells, atomics).
