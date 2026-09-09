# An itemized `is Array` subclass element rendered as its class name

`my @h = $c`, with `$c` holding an `is Array` (or `is List`) subclass instance,
made `@h.gist`, `@h.Str` and `@h.join` all answer `SA()` — the class name in
type-object shape — instead of rendering the element. `say @h` (the *implicit*
gist) was correct throughout, which is what kept the divergence hidden.
Reported as [#7660](https://github.com/tokuhirom/mutsu/issues/7660), found while
fixing [#7594](https://github.com/tokuhirom/mutsu/issues/7594).

```
$ raku  -e 'class SA is Array {}; my $c = SA.new(3,2,1,4); my @h = $c; say @h.gist'
[[3 2 1 4]]
$ mutsu -e 'class SA is Array {}; my $c = SA.new(3,2,1,4); my @h = $c; say @h.gist'
SA()          # before
[[3 2 1 4]]   # after
```

## Root cause

Not the `is Array` delegation machinery the issue suspected, and not a wrong
invocant: the receiver was the array all along. The trigger is **itemization**.

`my @h = $c` compiles an `ItemizeVar` for the right-hand scalar, so the array's
one element is a `Scalar` wrapping the instance, not the bare instance. Three
independent probes decide whether rendering a list needs the interpreter — the
answer must be "yes" whenever an element may carry a user-defined `gist`/`Str`,
because the pure renderers cannot dispatch a method — and all three looked
through a `:=`-bound `ContainerRef` cell but **not** through that `Scalar`:

- `collection_contains_instance_seen` (`runtime/methods_call_dispatch.rs`), the
  guard on the dispatching collection-gist renderer;
- `element_needs_interpreter` (`runtime/list_element_stringify.rs`), the guard
  on the `.Str`/`.Stringy` element resolver;
- the native `join` fast path (`builtins/methods_narg/dispatch_1arg.rs`), which
  declines to runtime when an element is an instance.

Each therefore answered "no interpreter needed" and the pure renderer printed
the generic `ClassName()` fallback for the element. The builtins-side gist twin
(`gist_route` in `dispatch_core_repr`) has always looked through both wrappers,
so the *native* path correctly declined — and then the runtime path it deferred
to declined to take over. That disagreement between the two halves is exactly
what produced a wrong answer rather than merely a slow one.

`say @h` escaped because the implicit gist does not go through the explicit
method-dispatch entry, and `.raku` escaped because its own probe
(`contains_dispatch_leaf_seen`) already descended itemization.

## Fix

Look through itemization at all three probes, and at the two resolvers that
follow them, so the probe and the resolver agree on what an element *is*:

- `collection_contains_instance_seen` gained `Scalar` and `ContainerRef` arms
  (with the cell's contents cloned out before recursing, so a cycle closing
  through a cell cannot deadlock against the lock `gist_item` holds), and cells
  now carry a container identity of their own in the visited set;
- `gist_item` gained the matching arms, dropping the itemization sigil exactly
  as its pure twin does;
- `element_needs_interpreter` and `resolve_elements` descalarize, so the
  element's own `Str` is dispatched;
- `dispatch_join_method` and the native `join` fast path descalarize for the
  same reason — `.join` stringifies each element with `.Str`, and a method call
  deconts its invocant.

## Pin

`t/array-subclass-element-gist-str.t` — 18 assertions covering implicit gist,
explicit `.gist`, `.Str`, `.join` and `.raku` for both an `is Array` and an
`is List` element, the nested case, and the four controls that were already
correct (a non-container instance element, a plain array element, the
instance's own gist/Str, and a *pushed* — hence non-itemized — element). The
file passes under `raku` unchanged.
