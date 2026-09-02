# `.VAR` hands back a container, not an opaque descriptor

`.VAR` in Raku returns the *container* — the real `Scalar` a mutable
`Array`/`Hash` element lives in, or the `Array`/`Hash` an `@`/`%` variable
names — and a container is transparent for ordinary method dispatch.

mutsu returned an attribute-only reflection object carrying `name`, `dynamic`,
`default`, `of` and an identity, and nothing else. Every method Raku answers
from the contained value came back out of an empty attribute map:
`@a[1].VAR.raku` was `Scalar.new` instead of `[3, 4]`, `.elems` was `1`
instead of `2`, `.VAR[0]` was `Nil`. The same held for a plain variable —
`my $z = [3,4]; $z.VAR.raku` was `Scalar.new` — and for `@a.VAR.raku`, which
answered `Array.new` for a two-element array.

[ADR-0064](../../docs/adr/0064-var-descriptor-carries-the-contained-value.md)
keeps the descriptor as the representation and gives it the one thing it was
missing: the value its container holds. The descriptor answers its own small
set of properties (name, dynamism, declared default and element type, identity
and type reflection, and `defined` — a container object is always concrete,
so `my @a; @a[0].VAR.defined` is `True` even though the element is `Any`) and
delegates everything else to that value. An element descriptor records the
element it describes; a variable descriptor records the variable's shared
`ContainerRef` cell when it has one — so reads through it stay live — and
otherwise the value the VM handed `.VAR`, refreshed in place on every call so
the cached instance keeps its ADR-0057 identity.

Two container-aware spellings fall straight out of ADR-0040's itemization,
because the recorded value is already itemized: `.gist` shows the *container*
(`@a[1].VAR.gist` is `$[3, 4]`) while `.raku` shows the *contained value*
(`[3, 4]`). And because a `Scalar` is not `Positional`, subscripting a
descriptor follows the one-item rule — `@a[1].VAR[0]` is the container's
content, `@a[1].VAR[1]` is an `X::OutOfRange` over `0..0` — rather than
reaching a level too deep.

Raku's `is [1,2,3][1].VAR, 2` passes because binding the container to a
parameter decontainerizes it; mutsu's `Test` builtins and `~` reach
`Value::to_string_value()` directly, so one arm in the Value renderer applies
the same rule there.

## A subscript's parent does not need a name

The descriptor is built from the *source container*, which the compiler names.
A chained subscript (`%d<a><b>`, `@g[0][1]`) has none to give: its parent is
the intermediate value `%d<a>`, which lives under no variable, so mutsu
answered the element's own type (`Int`) instead of `Scalar`. Raku still answers
from the parent — `[1,2][0].VAR` is `Scalar` while `(1,2)[0].VAR` is `Int` —
so the compiler now puts the parent on the **stack** next to the element
instead of naming it, and `Dup` after compiling it is what keeps this to one
evaluation even when the parent has side effects (`bump()[0].VAR`). Everything
the descriptor would have read from the variable degrades to what raku reports
for an anonymous container: `.name` is `element`, `.dynamic` is `False`,
`.default` is `(Any)`, `.of` is `(Mu)`.

## Four element-`.VAR` spellings that now reach the container

- **Multi-dimensional subscripts.** `@sh[0;0]` reaches the compiler as a
  different AST node than `@a[0]`, so `.VAR` never entered the element path at
  all and answered `Any`. Both spellings now route through one helper. A
  shaped array's element containers are anonymous in Rakudo, so their `.name`
  is `element`, not `@sh`.
- **`is default` and `of`.** `my @nat is default(0) = 1,2;
  @nat[0].VAR.default` was `(Any)`; an element now inherits its container's
  declared default and element type, read the same way the variable path reads
  them.
- **Native arrays.** `my int @a` stores unboxed values, so its elements have
  no `Scalar`: Raku hands back `IntPosRef`/`UIntPosRef`/`NumPosRef`/
  `StrPosRef`, which carry none of `Scalar`'s container properties. mutsu now
  mints the same classes, and drops the container attributes so `.of` and
  friends fail there as they should.
- **Slices.** `@a[0,1].VAR.^name` is `List` in Raku — a slice hands back a
  `List` *of* containers, and `.VAR` on a `List` is identity. The compile-time
  gate that shipped with the `is default` fix catches the statically
  recognizable spellings; a value-side discriminator now catches the rest
  (`@a[*]`, and an index whose runtime value happens to be a list), and only
  because of ADR-0040: a real container itemizes every element it stores, so a
  bare, non-itemized `List` arriving at the descriptor never came out of one
  element slot.

That last point makes ADR-0040's itemization load-bearing for *reflection*,
not just for rendering and flattening — which promptly exposed a hole in it.
A lazy source assigned to an `@` variable reified bare elements
(`my @l = lazy gather { take $_ for 1, (2,3) }; @l[1].raku` was `(2, 3)`, not
`$(2, 3)`) — closed the same day, see
`news/2026-09/lazy-array-elements-are-itemized-at-the-force.md`. Extending `.VAR` to multi-dimensional
subscripts also turned up
`todo/tickets/hash-multidim-subscript-assignment-rejected.md`: `%h{1;2} = 5`
is still rejected as an invalid multi-dim assignment, while the array spelling
works.

Pinned by `t/var-container-descriptor.t`, whose 62 assertions pass under real
`raku` as well as under mutsu.
