# A `$` attribute assigned an `@array` / `%hash` now shares it

`$obj.w = %src` used to store a detached copy, so a later `%src<y> = 2`
never showed through `$obj.w`, and `$obj.w.push(...)` never reached `@src`
(#9041). In Raku a `$`-sigil attribute is a Scalar holding the very object it
was given, exactly like `my $s = %src`.

Two things were wrong. First, an actual copy hid in the store:
`decay_nil_container_elements` ran `Gc::make_mut` whenever the container had
a non-`Nil` default, deep-copying every shared backing store even when no
element was `Nil`. It now looks for a `Nil` before touching anything, so
every container store keeps its identity unless it really rewrites an element.

Second, keeping the identity is not enough on its own: a later `@src.push`
copy-on-writes away from a plain shared `Gc`. So the attribute store now
joins the Slice 2a cell sharing of `docs/scalar-array-sharing.md` as its
Slice 2e. The `CallFunc` already names the value argument's source variable,
so after a successful store into a `$` attribute that kept the source's
backing store, the source is promoted to a shared `ContainerRef` cell and the
attribute holds the same cell in its itemized flavour. That flavour doubles
as the "value share, not `:=` bind" marker, so `$obj.w = 5`, `$!w = 5` inside
a method, and an assignment through `my $x := $obj.w` rebind the attribute
instead of overwriting `@src`, while `$obj.w[0] = 9` and `$obj.w<k>:delete`
write through to it.
