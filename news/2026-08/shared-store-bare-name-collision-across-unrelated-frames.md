# Two unrelated frames no longer collide through the cross-thread container lane

A spawn publishes every live `@`/`%` lexical into the bare-name-keyed
cross-thread store. The entry then outlived the frame that owned it, and any
later frame with a same-named container resolved to it instead of to its own
binding — a silent wrong value, fully deterministic, repeating on every call.
`start`/`Promise` armed it; one `await start { 1 }` anywhere in the process was
enough.

```raku
sub work($tag) {
    my @items = ($tag,);
    await start { 1 };          # delete this line and mutsu was correct
    @items.push("$tag-2");
}
my @items = <x y z>;
work('A');  say @items.raku;    # raku: [x y z]        was: [A A-2]
@items.push('MINE');            # raku: [x y z MINE]   was: [A A-2 MINE]
work('B');  say @items.raku;    # raku: [x y z MINE]   was: [B B-2]
```

A second shape, the same cause: a **non-slurpy `@`/`%` parameter** escaping its
call, because `mask_thread_redeclared_params` masks only scalars and slurpies.

```raku
sub takes(@list is copy) { await start { 1 }; @list.push('R') }
my @list = <x y z>;
takes(<p q>);
say @list.raku;                 # raku: [x y z]        was: [p q R]
```

Both reproduced identically with `%`, and the first also reproduced through a
`use`d module (the module routine's local `@parts` overwriting the consumer's
`@parts` — the mirror image of ADR-0039 §1.1, where the consumer overwrote the
module).

## What the ticket's premise turned out to be

This started life as "`shared_vars` is a **process-global** map keyed by bare
name, so re-key the store". Two thirds of that expired before it was worked:

- **The keying fix already shipped.** ADR-0010 replaced the one process-wide map
  with the lineage-chained `SharedStore` (`src/runtime/shared_store.rs`), so
  sibling spawns no longer see each other's lexicals.
- **Scalars are clean.** Ten shapes were probed on `52631889f` and every one
  matched `raku`; the `thread_redeclared_vars` mask plus the Nil-gated `GetLocal`
  pull cover the scalar lane. A scalar reads its slot and consults the store only
  when the slot holds `Nil`; a container had no slot read at all, which is why
  only `@`/`%` were left.
- **Re-keying was rejected outright** (ADR-0039 §8.4 point 4): the surviving
  repro collides two frames of *one* thread inside *one* lineage, and the
  callee's `my @items` runs **before** the process's first spawn, so there is no
  mask to keep, re-key or scope. No discipline applied to the store can fix it.

What was left was a single sigil, and it was not a keying problem at all — see
below.

## The fix: the lane entry gets a lifetime

A bare-name entry can be wrong in two independent ways. It can name the wrong
binding — a *keying* problem, and ADR-0039 §8.4 point 4 is right that only slots
fix that. Or it can be **alive when it should not be**. The repros above are the
second one: the seeding loop publishes *everything* live at every spawn, so a
frame-local container lands in a process-visible store, and the entry then
outlives the frame it belongs to. That needs no key redesign; it needs the entry
to stop existing when it stops being needed.

So lane entries now have a lifetime:

- `block_referenced_containers` (new, `src/runtime/runtime_thread.rs`) collects
  the plain-lexical `@`/`%` names in the spawned block's `free_var_syms` /
  `free_var_writes` / `free_var_container_writes` — which already fold up nested
  closures, so `start { start { @a.push(1) } }` counts `@a`. It returns `Option`
  and is `None` for the block-less `clone_for_thread` entry point (supply
  drivers, `.then`, socket and proc readers), which keeps its previous behaviour
  exactly.
- `clone_for_thread_excluding` classifies each entry as it publishes it. A
  container the block **names** is genuinely shared: durable, and any earlier
  transient mark on it is cleared. A container it never names, **whose entry
  this spawn created**, is recorded in `transient_lane_containers`. Only entries
  this spawn created are marked, so one an earlier naming spawn established
  stays durable however many unrelated spawns walk past it later.
- `sync_shared_vars_to_env` withdraws the marked entries (and their
  `__mutsu_atomic_*` twins, which reads prefer) at the tail of the drain, after
  everything the workers did has been merged back into `env` or written through
  the owning unit-lexical cell. It first *materializes* a dirty entry's live
  value into the frame's own storage — once a mutation has routed through the
  lane the atomic entry is the authoritative copy, the mutating thread
  deliberately dropped its `env` copy, and the ordinary drain cannot restore it
  because its dirty-key filter skips any name this lineage re-declared.
- Only the **top-level** interpreter classifies. On a worker thread the lane is
  not an optional publication channel — it is the storage: `push @a, ...` routes
  through `__mutsu_atomic_arr::` unconditionally when `is_thread_clone()`
  (`src/vm/vm_data_push_ops.rs`), precisely so concurrent appends serialize.
  Retiring an entry there withdraws a deliberate mechanism's backing store
  mid-use (measured: it emptied worker A's accumulator in
  `t/sibling-thread-array-lane-scope.t`), and buys nothing — a worker's lineage
  store is its own (ADR-0010), so its entries cannot outlive into an unrelated
  frame the way a root-store entry from the main interpreter does.

Withdrawing at the drain rather than declining to publish is the load-bearing
choice, and it was arrived at by measurement.

## Two cheaper routes, measured and rejected

Both were implemented and run through full CI before the one that shipped. The
results are recorded here and in ADR-0039 §8.6 so nobody re-derives them.

**(1) Decline to seed a container the block never names** — the direct reading
of ADR-0039 §8.3's "third `@`/`%` sigil skip". It fixed both repros and left the
entire rest of the `t/` suite green (3341 files, 31116 assertions), failing
exactly two assertions, both **indirect**: a worker whose block names only a
routine (`await start { inner('x') }` where `sub inner { @acc.push(...) }`, and
its mainline-named-sub twin) that pushes to an outer container. Those containers
really are shared and the name lane really is what carries them. A static
reachability analysis over the block's own free variables cannot see through a
call, so this is not merely incomplete — it is the wrong instrument.

**(2) Resolve the entry by container identity** — replace the
`thread_redeclared_vars` mask with "is the store's node the same `Gc` as the one
this frame holds?", on the reasoning that container mutation is
write-through-the-shared-node so a container's node *is* its binding. It fixed
everything above, and is unsound under contention: `Gc::make_mut` inside
`shared_array_mutate` reallocates whenever the node is shared, so a concurrent
mutation destroys the identity the test depends on and a reader whose copy has
drifted silently writes locally. Measured: `t/concurrent-array-index-assign.t`
and `t/concurrent-hash-assign.t` lost updates under heavy contention (20 threads
× 50 indices), `t/escaped-closure-elem-incdec-delete.t` failed, and
`t/cas-multidim-cells.t` timed out. This is exactly the failure mode CLAUDE.md's
risk definition warns about — correct only under an analysis that is incomplete
in precisely the concurrent case.

## Pins

`t/thread-uncaptured-container-lane.t` (21 assertions, all verified against real
`raku`): both repros above and their `%` twins, plus the sharing that must
survive — a container the block names, concurrent pushes, concurrent element
assignment, a nested spawn, sibling-worker isolation, and both indirect shapes
(a nested named sub and a mainline named sub pushing from a worker).
`t/thread-callee-array-does-not-clobber-caller.t` and the rest of the
concurrency suite stay green.

## What is still open

Container scoping is still *dynamic* in the compiler: `Expr::ArrayVar` emits a
bare `GetArrayVar(name)` (ADR-0039 §1.3), and the four by-name mechanisms listed
in ADR-0039 §8.4 point 3 are still in place. A container reached only indirectly
is still shared by bare name for the life of the spawn, so two frames that are
both live and both spawning while sharing a name can still collide inside that
window. ADR-0039 slice 2 remains the end state and subsumes all of it — once a
container resolves through its slot, no entry lifetime needs managing. What
changed is that the recorded, reproducible failure mode is closed and pinned.
See ADR-0039 §8.6.
