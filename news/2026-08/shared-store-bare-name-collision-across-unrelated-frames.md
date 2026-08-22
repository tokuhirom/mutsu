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

What was left was a single sigil, and it was not a *keying* problem: it was
that a bare name cannot say which binding an entry belongs to.

## The fix

The cross-thread store is keyed by bare name, so by itself it cannot tell "the
container this frame is looking at" from "some other frame's container that
happens to share the name". `container_name_is_redeclared`
(`src/runtime/runtime_shared_vars.rs`) is the predicate all nine lane gates
already consult for that question, and its old answer -- "was this name masked
by a `my` since the last spawn?" -- is structurally incapable of covering the
repros above:

- the mask is only ever populated while `shared_vars_active`, so the callee's
  `my @items`, which runs *before* the process's first spawn, is never masked at
  all; and
- the mask is not scoped to the declaring frame, so even when it is set it is
  dropped at the next spawn and outlives the frame when it is not.

It now also asks the containers themselves. Container mutation in mutsu is
write-through-the-shared-node (ADR-0013 §7 / ADR-0039 §2), so a container's `Gc`
node **is** its binding identity -- the same property slots would give, read off
the value instead of the frame. `container_store_binding_is_foreign` resolves
the name the way the frame would without the store (`unit_lexicals` first, then
`env`) and compares its node against the store's base entry and its
authoritative `__mutsu_atomic_*` copy. A match means the entry is about *this*
binding and every lane preference is correct; no match means it belongs to
another frame and this one stays local.

The test is conservative in the direction that preserves sharing: no live local
binding, a non-container value, or a name absent from the store all answer "not
foreign", leaving the previous behaviour exactly as it was. It is restricted to
plain lexical names, so twigil'd, dynamic (`@*x`), attribute and `::`-qualified
containers keep their own routes, and it only runs while `shared_vars_active`.

### The approach it replaced, and why

ADR-0039 §8.3 pointed at a "third `@`/`%` sigil skip": `block_captured_scalars`
`continue`s on `@`/`%`/`&` while scanning a spawned block's free variables. The
direct reading of that is to stop *publishing* a container the spawned block
never names. That was tried first. It fixed both repros and left the entire rest
of the `t/` suite green (3341 files) -- but broke exactly two shapes, both
**indirect**: a worker whose block names only a routine
(`await start { inner('x') }`) that pushes to an outer container. Those
containers really are shared, and the name lane really is what carries them, so
a static reachability analysis over the block's free variables is the wrong
instrument: it cannot see through a call.

The identity test needs no reachability analysis. In the indirect shapes the
store's entry *is* the frame's own container, so it answers "not foreign" and
the sharing stands; in the collision shapes it is a different container, so the
frame stays local.

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
