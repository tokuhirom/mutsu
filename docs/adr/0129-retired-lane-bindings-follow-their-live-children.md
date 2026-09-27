# ADR-0129: A re-declared lane binding is retired into a box its live children keep

- Status: Accepted (implemented)
- Date: 2026-09-27
- Refines: ADR-0010 (cross-thread lexical sharing is scoped to a spawn lineage)
- Related: ADR-0023 (binding provenance at spawn), ADR-0039 §8.6 (transient
  lane entries), ADR-0055 (a closure's free variable resolves to its own binding)
- Resolves: [#9723](https://github.com/tokuhirom/mutsu/issues/9723)

## Context

ADR-0010's store is keyed by bare name and holds one entry per name per
lineage. A routine that declares `my @p` and spawns a thread (`start`, or a
`.then` on a pending promise) publishes `@p` into the spawning lineage at the
spawn. When the routine is called again, its new `my @p` is published under the
same key in the same lineage, and the first call's thread — still running —
resolves the name through its chain to the *new* binding:

```raku
sub h($n) { my @p = ^$n; start { sleep 0.2; @p.elems } }
say await h(1), h(2), h(3);   # raku: (1 2 3)   mutsu was: (3 3 3)
```

The child reads the lane whenever the name is dirty (any thread's write marks
it, including the parent's next binding) and whenever it mutates the container
(the `__mutsu_atomic_*` lanes resolve at the lineage owning the base name).
Scalars mostly escape because a plain captured scalar is a cell kept off the
lane entirely (`block_captured_scalars`); `@`/`%` aggregates cannot be taken off
the lane, because it is what makes `my @a; await start { @a.push(1) }; @a`
work — the child's writes reach the parent through it.

The issue offered three directions: capture aggregates as cells, key the store
by declaration, or make every capture of a thread-run closure authoritative.
The first changes how every aggregate mutation reaches another thread and
needs the complete capture analysis ADR-0010 §B says mutsu does not have. The
third does not reach the lane at all (the child reads the store, not the
caller's env). The second is what the data actually needs — but only for the
bindings that a live child still holds, which is a much smaller thing than
re-keying the whole store.

## Decision

**When a lineage is about to replace or clear its entry for a user lexical,
the entry is retired into a binding box, and every live child that captured
it is redirected to the box.**

- `SharedStore::child_of` registers each child with its parent (a `Weak`, in
  spawn order, with a spawn sequence number).
- `SharedStore::retire_binding(key)` runs before the two operations that end a
  binding's life in its lineage: `declare` (the spawn-time publication of a
  re-declared name, which overwrites) and the declaration-time lane clear
  (`clear_atomic_array_state` / `clear_atomic_hash_state`, which deletes the
  `__mutsu_atomic_*` twin). If the lineage owns `key` and has live children
  spawned since `key` was last retired there, the entry and its lane twin move
  into a fresh box and each such child gets `redirects[key] = box`. With no
  live child the entry is left exactly as before.
- Lookup consults a lineage's redirects after its own map and before its
  parent (`holder_here`), so `get`, `set`, `with_entry_mut`, `remove`,
  `contains_key`, the lane routing (`scope_for`, `atomic_lane_scope`) and the
  GC root walk (`chain_values`) all see the box as the owner of the name.

All children of one old binding share one box, so they keep seeing each
other's writes after the re-declaration; grandchildren resolve through their
parent's redirect. A box is a leaf store owning one name and its lane twin.

### The in-flight declaration re-masks at its store

The Tinky hang needed one more piece. `my @promises = helper(0), helper(1),
helper(2)` runs callees that each declare their own `my @promises` and spawn.
`thread_decl_in_flight` and `thread_redeclared_vars` are keyed by bare name,
so the callee's initialising store ends the *caller's* in-flight window, the
callee's spawn then drops the caller's mask, and the caller's store wrote its
new binding with a plain `set` — straight into the entry the last callee's
`.then` child was reading, which then waited on its own promise.

A declaration's binding is unpublished until the next spawn after it,
whatever its initializer did, so the store that completes a declaration
(`SetLocal` in declaration context) now re-inserts the mask
(`remask_declaration_store`). The following spawn `declare`s the binding as
usual, and that `declare` retires the callee's entry into a box for its child.
The eligibility test is shared with `SetVarDynamic`
(`thread_decl_masks_name`) so the two cannot drift.

## Alternatives considered

- **Cells for aggregates.** The destination ADR-0010 §B names; still not
  chosen for the reason given there. This ADR does not move away from it: once
  captured aggregates are cells, retirement has nothing left to retire.
- **Fork the parent's lineage on re-declaration** (the parent moves to a new
  child store, old children keep the old one). Correct, but the chain grows by
  one level per re-declaration while children live, and every lookup of an
  outer name walks it: a loop that spawns per iteration becomes O(n) per read.
  Retirement keeps the chain depth equal to the spawn nesting.
- **Copy the old value into each child's own map.** Cheaper to write, but two
  children of one binding would then diverge after the re-declaration.

## Consequences

- Cost: `child_of` is O(1) amortised (dead registrations are pruned when the
  list doubles). `retire_binding` is O(c), c = children spawned since the
  name's last retirement in that lineage, and runs only on a re-declaration
  whose name the lineage actually holds. Lookups pay one atomic load per
  chain level for the redirect fast-path guard.
- A binding box lives as long as a child that redirects to it.
- The parent-side counterpart — a *recursive* frame reading its own `@p` after
  a nested call re-declared it — is not addressed here; the parent reads its
  env, not the lane, while the mask holds.

## Validation

- `t/concurrency/thread-lock/thread-closure-own-aggregate-binding.t` pins the
  issue's `start` / `.then` array and hash repros, writes after the
  re-declaration, sibling sharing through one box, the in-flight caller case
  (with a timeout instead of a hang) and ordinary outer-array sharing.
- The thread/shared/atomic/promise/supply/closure `t/` files, `make test` and
  `make roast` through `scripts/dev gate`.
