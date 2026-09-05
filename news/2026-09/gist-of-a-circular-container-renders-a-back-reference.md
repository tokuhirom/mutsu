# `.gist` of a circular container renders a back-reference instead of aborting

`say @c` and `@c.gist` on a self-referential container took the whole process
down — `thread 'mutsu-main' has overflowed its stack / fatal runtime error:
stack overflow, aborting`, not an exception a `CATCH` could see:

```raku
my @c;
@c = 42, @c;
say @c.raku;   # ((my @Array_140…) = [42, @Array_140…])  -- fine
say @c.gist;   # process aborts
```

mutsu now renders what Rakudo renders:

```
(\Array_140723… = [42 Array_140723…])
```

## The originating ticket's root cause was wrong

The ticket read the crash as "the `.raku` walk has cycle detection and the
`.gist` walk has none". `gist_value` (`src/runtime/utils/gist.rs`) *did* have a
visited set. The overflow never reached it.

Five separate walks stand between `say @c` and a rendered string, and four of
them were plain recursions with no cycle guard at all. Each was found by
breaking on the faulting frame under `rust-gdb` — one guess would not have
found them, because fixing any one of them just moved the crash to the next:

1. `collection_contains_instance` (`src/vm/vm_native_dispatch.rs`) — the VM's
   probe deciding whether the native gist fast path applies.
2. `gist_needs_method_dispatch`
   (`src/builtins/methods_0arg/dispatch_core_repr.rs`) — the same decision in
   the builtins layer.
3. `element_needs_method_dispatch` (`src/vm/vm_data_io_ops.rs`) — the `say`
   fast path's own probe, which is why `say @c` crashed as well as `.gist`.
4. `collection_contains_instance` again (`src/runtime/methods_call_dispatch.rs`)
   — a second, differently-spelled copy on the interpreter side.
5. The native per-receiver-type renderer in `dispatch_core_repr` itself, a
   family of local `gist_item` / `gist_real_array_item` recursions.

Walks 1, 3 and 4 now carry the same visited-set-plus-depth-cap discipline their
`.raku` twin `contains_dispatch_leaf_seen`
(`src/runtime/methods_raku_dispatch.rs`) has had all along, keyed on the `Gc`
node pointer. Walk 5 is not given a sixth copy of that discipline: a cyclic
receiver is instead routed to `gist_value`, the one renderer that owns the
cycle rule.

Walk 2 became `gist_route`, which answers *both* questions — "does an element
need dispatch?" and "is this cyclic?" — in a single pass. That matters for more
than tidiness: `.gist` renders at most 100 elements while a probe walks the
whole structure, so on a large receiver the probes cost more than the rendering
they guard. An intermediate version of this fix ran the cycle check as a
separate pass; merging the two passes, and dropping some incidental `to_vec()`
copies the probes did not need, cut a large-array `.gist` back to roughly half
that version's cost in a local A/B — so the shipped shape adds no walk over what
`gist_needs_method_dispatch` already did.

## A deadlock behind the overflow

Merging the passes surfaced a second, independent bug. `my @e; @e.push(@e)`
stores a `ContainerRef` cell that reaches the array again, and three renderer
arms — two `gist_item` arms in `dispatch_core_repr` and `gist_value`'s own —
were written as `gist_item(&cell.lock().unwrap())`, i.e. they held the cell's
mutex *across* the recursive call. Reaching the same cell twice therefore
blocked on the futex instead of recursing: a silent hang, not a crash, with
`perf` recording zero samples because nothing was burning CPU.

The cycle probes had been looking straight past cells, so all three arms now
clone the contents out and drop the guard before recursing, and a cell carries a
container identity of its own in both `gist_route` and `contains_cycle` — a
`:=`-bound element is a legitimate way for a cycle to close.

## The rendering, measured rather than assumed

The ticket also asserted that "rakudo renders `[42 [...]]`". It does not. Run
against real `raku`, Rakudo uses `Mu.gistseen`: on entry a node is marked, on a
revisit it is named `<Type>_<address>` and the mark is bumped, and on exit a
bumped node wraps itself in a `(\Name = ...)` binding preamble. Two consequences
that a `[...]` placeholder would have got wrong, both now pinned:

- **The preamble sits on the node the walk loops back to, not the top level.**
  `my @a; my @b; @b = 1, @b; @a = 0, @b` gists as
  `[0 (\Array_… = [1 Array_…])]`.
- **A DAG is not a cycle.** The same container reachable by two *non-nested*
  paths is rendered in full at each occurrence, so the visited set is
  ancestor-scoped (pushed on entry, popped on exit), not walk-global.

`.Str` / `~` / `.join` were checked at the same time, as the ticket suggested.
They are deliberately left alone: Rakudo has no cycle rule there either and
`raku -e 'my @c; @c = 42, @c; say @c.Str'` hangs.

Pinned by `t/gist-circular-container.t` (11 rows), which passes identically
under `raku` and mutsu.

## Known remaining gap

An element carrying a user-defined `method gist` *inside a cyclic structure*
renders with its default gist, because `gist_value` is pure and cannot dispatch
into the interpreter. A `TODO` marks the spot. A crash-free default gist is the
better trade until the interpreter-side walk carries the same visited set.

A second, unrelated divergence surfaced while building the shape matrix and is
filed separately as
`todo/tickets/hash-self-assignment-copies-instead-of-aliasing.md`: `%h<self> =
%h` does not build a real cycle in mutsu at all — it deep-copies and leaves a
`__mutsu_self_hash_ref` marker that leaks into both `.gist` and `.raku`. A cycle
that reaches a hash *through* an array (`@m = 1, %n; %n<x> = @m`) is genuine and
renders correctly.
