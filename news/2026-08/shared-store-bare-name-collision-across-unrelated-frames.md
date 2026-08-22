# Two unrelated frames no longer collide through the cross-thread container lane

A spawn used to publish **every** live `@`/`%` lexical into the bare-name-keyed
cross-thread store, including containers the spawned block could not possibly
reach. The entry then outlived the frame that owned it, and any later frame with
a same-named container resolved to it instead of to its own binding — a silent
wrong value, fully deterministic, repeating on every call. `start`/`Promise`
armed it; one `await start { 1 }` anywhere in the process was enough.

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
  pull cover the scalar lane.
- **Re-keying was rejected outright** (ADR-0039 §8.4 point 4): the surviving
  repro collides two frames of *one* thread inside *one* lineage, and the
  callee's `my @items` runs **before** the process's first spawn, so there is no
  mask to keep, re-key or scope. No discipline applied to the store can fix it.

What was left was a single sigil, and it was not a store problem at all: the
spawn published a binding nothing had asked to share.

## The fix

ADR-0039 §8.3 had already named the mechanism — `block_captured_scalars`
(`src/runtime/runtime_thread.rs`) `continue`s on `@`/`%`/`&` while scanning a
spawned block's free variables, throwing the container half of that analysis
away — and called it "a third instance of the `@`/`%` sigil skip". That skip is
now lifted, and the recovered information used with the polarity the container
lane actually has:

- `block_referenced_containers` collects the plain-lexical `@`/`%` names in the
  spawned block's `free_var_syms` / `free_var_writes` /
  `free_var_container_writes` (these already fold up nested closures, so
  `start { start { @a.push(1) } }` keeps `@a`). It returns `Option`, and is
  `None` for the block-less `clone_for_thread` entry point (supply drivers,
  `.then`, socket and proc readers), which is therefore unchanged.
- `clone_for_thread_excluding` **does not seed** a plain-lexical container the
  block never names, and **keeps** such a container's existing
  `thread_redeclared_vars` mask across the spawn rather than dropping it. The
  retain half covers the composite shape — an outer binding already on the lane,
  plus a callee that re-declares the name and spawns something unrelated —
  where the unmask would otherwise hand the callee's fresh `my @items` straight
  to the caller's live entry.

The polarity is inverted relative to scalars, and deliberately so. For a scalar,
"the block captures it" means the closure machinery owns it per binding, so it
is kept **off** the lane (the lane would be a competing, lossy second mechanism).
A container has no per-binding home — `box_captured_lexicals` declines to box
`@`/`%`, ADR-0025 slice 3's deferral — so for containers the lane *is* the
sharing mechanism: a container the block names belongs on it, one it never names
does not.

A container reached only **indirectly** — by a routine the block calls rather
than names — needs no lane entry either. Container mutation is
write-through-the-shared-node (ADR-0013 §7 / ADR-0039 §2) and the child's env
clone holds the same `Gc`, so a worker's push is visible without a store entry;
and when a directly-nested named sub mutates the container, the declaration site
has already boxed it into a shared `ContainerRef` cell
(`box_decl_local_container_cell`).

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
in ADR-0039 §8.4 point 3 are still in place. ADR-0039 slice 2 remains the end
state — but it is now a mechanism-**deletion** slice rather than a correctness
one. See ADR-0039 §8.6.
