# ADR-0023: Spawn-time capture ownership is decided by binding provenance, not value type

- Status: Proposed
- Date: 2026-08-10
- Refines: ADR-0010 (cross-thread lexical sharing scope)
- Resolves: `todo/deep/concurrent-for-loop-siblings-cannot-share-a-bare-loop-param-name.md`

## Context

`for $client-a, $client-b -> $client { start { ... $client ... } }` spawns two
concurrently-live threads whose `$client` bindings are distinct, but — when the
loop items are not on `block_captured_scalars`' "plain" scalar allow-list
(`src/runtime/runtime_thread.rs:43-61`; an `Instance` is the practical case) —
both threads converge on one value. The deep-dive ticket
(`todo/deep/concurrent-for-loop-siblings-cannot-share-a-bare-loop-param-name.md`)
established the root cause with instrumented builds:

1. The single-param `for` loop binds its parameter by a direct env write
   (`vm/vm_for_loop_body.rs:438`), never through the `my`-declaration opcode.
2. `block_captured_scalars` treats a non-"plain" scalar as **not owned by the
   closure machinery**, so the bare name stays on the ADR-0010 name-keyed
   shared-store lane: each spawn seeds its current value under the ONE key
   `client` in the **spawning (parent) lineage**, and each spawned child pulls
   that key back over its env at every `await`
   (`sync_shared_vars_to_env`) — last writer wins for everybody.
3. Masking (`thread_redeclared_vars`, the multi-param loop's `#6081`
   mechanism) cannot fix this and was empirically shown not to (ticket,
   "What was tried and reverted"): masking only selects WHICH seeding branch a
   spawn takes (`shared.declare` force-overwrite vs `seed_if_absent`); both
   branches still funnel two simultaneously-live values into one name-keyed
   slot.

### The insight this ADR is built on

The "plain" allow-list is a **type-based proxy** for the question that
actually matters: *does the closure machinery own this binding per-instance,
or is the name lane the only thing keeping parent and worker coherent?* For
an enclosing-scope `my $c = Channel.new` captured by a `start` block, the
lane is genuinely load-bearing: it propagates variable **rebinds** between
parent and worker (mutsu's COW value semantics mean a write can replace the
`Gc` behind a name, and the lane is what re-associates the name with the new
value across threads).

A **for-loop parameter is categorically different**, independent of its type:

- It is a **fresh per-iteration binding** (Raku semantics; the loop body even
  marks it readonly via `mark_readonly`, `vm_for_loop_body.rs:461-467`,
  unless `is rw`/`is copy`).
- No rebind of it ever happens that another thread must observe: the parent's
  next-iteration re-bind is precisely the write the spawned child must NOT
  see, and the child never assigns to it.
- Object-interior mutation does not need the lane: `Instance` attribute
  writes go through `Gc`-backed interior mutability (`arc_contents_mut`),
  `SharedChannel` is `Gc<(Mutex<ChannelState>, Condvar)>`
  (`src/value/mod.rs:2246`) — a spawn-time env-clone shares the same heap
  object, so `.send`/attribute writes remain visible both ways without any
  name-lane traffic.

So the spawn-time env snapshot (which `clone_for_thread_excluding` already
takes — the child's env IS a clone of the parent's at spawn time, holding the
correct per-iteration value) is a fully correct ownership form for a loop
parameter **of any type**. The bug is solely that the name is *additionally*
run through the lane, whose seed/pull traffic then overwrites the correct
snapshot.

## Decision

Add a **binding-provenance axis** to the spawn-time capture decision: a
scalar free variable of a spawned block that is currently bound as an
**active for-loop parameter** in the spawning frame is treated as
closure-owned (added to `block_captured_scalars`' result) **regardless of its
value's type**. The existing type-based "plain" rule is unchanged for every
other name.

Being in that set makes the existing machinery do the whole job, with no new
lane semantics:

- `clone_for_thread_excluding` **skips seeding** the name into the parent
  lineage (`runtime_thread.rs:239-243` — the skip runs *before* the
  declare/seed_if_absent branch selection, which is why this succeeds where
  the reverted masking attempts could not).
- The child clone **inherits the name as masked**
  (`thread_redeclared_vars: captured_scalars...`, `runtime_thread.rs:556`),
  so `sync_shared_vars_to_env` never pulls a lane entry (e.g. a stale value
  from an earlier same-named `given`/`my` warm-up) over the child's correct
  env-clone value.
- The parent-side mask-retain (`runtime_thread.rs:324-328`) keeps any
  pre-existing mask for the name, which is harmless: subsequent sibling
  spawns also skip seeding, so the force-`declare` overwrite war observed in
  the ticket can no longer trigger.

No change to `box_captured_lexicals`, no `ContainerRef`-wrapping of
Instances, no ValueView-deref audit, no `SharedStore` structural change.

## Mechanism (implementation plan)

### Step 1 — track active loop-parameter names

Add to `Interpreter` (field block in `src/runtime/mod.rs`, near
`loop_local_vars`):

```rust
/// Names currently bound as for-loop parameters in this frame chain, one
/// set per active loop (ADR-0023). Bare names (no `$` sigil), matching
/// env keys. Consulted by `block_captured_scalars` only; never persisted.
pub(crate) active_loop_param_names: Vec<rustc_hash::FxHashSet<String>>,
```

Initialize empty in `runtime_init.rs` and in `clone_for_thread_excluding`'s
child construction (`runtime_thread.rs` — the child starts outside any loop,
same as the other per-execution registers).

### Step 2 — push/pop with the loop lifecycle

In `vm/vm_for_loop_body.rs`, alongside the existing
`push_loop_local_scope()` / `pop_loop_local_scope()` pair (which already has
balanced push/pop on every exit path — reuse exactly those sites, or fold the
push/pop into `push_loop_local_scope`/`pop_loop_local_scope` themselves in
`vm/vm_control_ops.rs:149-…` so ALL loop forms get it for free; folding is
the preferred shape since it structurally guarantees balance):

- Pushed set contents: the single `param_name` plus every
  `spec.multi_param_names` entry, each with a leading `$` stripped, filtered
  like `masked_multi_params` (`vm_for_loop_body.rs:310-316`): skip
  `&`-sigiled names, skip `_`, and skip `@`/`%`-sigiled names (aggregates
  keep their existing lanes; see Non-goals).
- **Gate: if `spec.is_rw` is true, push an empty set.** An `<->`/rw loop
  parameter writes back to the source element; keeping it on its current
  path is the conservative choice and matches the pre-ADR behavior.
- Note: `while`/`until`/C-style loops have no loop parameter, so if the
  push/pop is folded into `push_loop_local_scope`, those callers push an
  empty set (pass the names in, defaulting to empty). `vm_for_loop_intrange.rs`
  (the specialized int-range `for` loop) must also pass its `param_name` —
  its items are Ints today (already "plain"), but the invariant should hold
  by construction, not by type coincidence.

### Step 3 — isolate across routine boundaries

`with_nested_registers` (`vm/vm_run_loop.rs:248-…`) already saves/takes
`loop_local_vars`, `for_param_restore_stack`, etc. Add
`active_loop_param_names` to that save/reset/restore list. This structurally
prevents the false positive where a routine *called from* the loop body
spawns a block whose free variable merely shares the loop parameter's name —
inside the callee, the stack is empty.

**Verification task for the implementer:** confirm that compiled-function
calls that execute within the SAME run loop (the fast call paths in
`vm_call_fast.rs` / `vm_call_named.rs`, which push `call_frames` without
entering `with_nested_registers`) either (a) do not reach a spawn site
without `with_nested_registers`, or (b) if they do, record
`self.call_frames.len()` in the pushed entry and have `block_captured_scalars`
apply the rule only when the current `call_frames.len()` equals the recorded
depth. Start with the simple `with_nested_registers`-only form; add the
depth check only if a test demonstrates the leak (write the probe test
either way: a loop body calling `sub helper($unrelated) { start { $client } }`
where the callee closes over an outer `my $client`).

### Step 4 — consult it in `block_captured_scalars`

In `src/runtime/runtime_thread.rs`, inside the free-variable loop, after the
`type_body_written_lexicals` exclusion (line 30) and **before** the "plain"
type check (line 43):

```rust
// ADR-0023: a name currently bound as a for-loop parameter is a fresh,
// readonly, per-iteration binding — the spawn-time env clone is its
// correct per-binding home for ANY value type. Keeping it off the lane
// is what lets two sibling iterations' spawns each hold their own value
// (todo/deep/concurrent-for-loop-siblings-…).
if self.active_loop_param_names.iter().any(|s| s.contains(bare)) {
    out.insert(bare.to_string());
    continue;
}
```

Both spawn entry points (`builtins_system.rs:176` for `start`,
`native_methods/scheduler.rs:541` for `cue` callbacks) go through
`clone_for_thread_for_block` → `block_captured_scalars`, so one change covers
both.

### Non-goals (explicitly out of scope, with follow-up filing)

- **`@`/`%`-sigiled loop parameters**: aggregates have their own lanes
  (`__mutsu_atomic_*`) and the `param_bound_aggregates` special case; they are
  a follow-up using the same provenance principle. File a
  `todo/tickets/` note if not picked up in the same PR.
- **Runtime-invoked callback scalar parameters** (`@xs.map(-> $client {
  start {...} })`): same theoretical gap, same future mechanism (record
  provenance at the env-level parameter binder). The existing
  `param_bound_aggregates` path is precedent. File as follow-up ticket with a
  repro; do NOT widen this ADR's slice to cover it blind.
- **`given`/routine parameters**: already handled by
  `exec_set_var_dynamic_op` masking and `thread_param_shadow_vars` (#6173);
  unchanged.
- **`Thread.start` / spawns without a known block** (`clone_for_thread`
  without `_for_block`): unchanged — no block, no capture analysis.

## Alternatives considered

- **(a-full) Extend "plain" to Instances / box everything per binding**
  (ticket option a): requires proving a `ContainerRef`-wrapped Instance
  behaves identically at every `ValueView`-matching site (the universal-deref
  gap is exactly ADR-0001's Track B, deliberately fused with GC and not to be
  started standalone), and would change identity semantics for the
  Channel-style "one shared object" captures the allow-list comment pins
  (t/concurrency-threading.t test 4). Rejected as over-broad; this ADR gets
  the same isolation for the failing class of bindings without touching value
  representation.
- **(b) Per-iteration sibling lineages in `SharedStore`** (ticket option b):
  the seed targets the *parent* lineage precisely so an enclosing `my` stays
  writable through the chain; splitting lineages per iteration would need a
  per-name decision of "seed into child vs parent" — i.e. the very same
  provenance judgment this ADR makes — plus new store machinery and a
  re-audit of `thread_param_shadow_vars`/`thread_redeclared_vars`
  interactions. Strictly more moving parts for the same discrimination.
- **Masking extensions**: empirically disproven twice (ticket, "What was
  tried and reverted"). Masking picks a seeding branch; it cannot make one
  name-keyed slot hold two live values. This ADR's skip happens *before*
  branch selection — no seed, no slot contention.

## Acceptance criteria

1. `raku`/`mutsu` output parity (`A,A,A,A,A | B,B,B,B,B`) on all four
   `tmp/repro-minimal-given-barename*.raku` files (they exist in `tmp/` as of
   2026-08-10; regenerate from the ticket if evicted).
2. New pin test `t/for-loop-param-start-sibling-isolation.t` covering: the
   warm-up variant, no-warm-up, renamed warm-up, plain-block warm-up, a
   **multi-param** variant (`for $a1,$b1,$a2,$b2 -> $x, $y { start {...} }`),
   and a Channel-typed loop item exercised cross-thread
   (`for Channel.new, Channel.new -> $c { start { $c.send(1) } }` + receive —
   verify against `raku` first).
3. No regressions: `t/concurrency-threading.t` (esp. test 4),
   `t/thread-shared-scalar-visibility.t`, `t/lock.t`,
   `t/supply-batch-period.t`.
4. `t/http-session-inmemory.rakutest` and `t/http-session-persistent.rakutest`
   subtests 8-9: `Visit 1..5` for both clients, un-interleaved.
5. `make test` locally; full `make roast` delegated to CI (this touches
   cross-thread visibility machinery — a local subset is not sufficient).

On completion, `git mv` the ticket to `news/2026-08/` per `todo/README.md`.
