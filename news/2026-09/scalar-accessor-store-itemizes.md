# A `$` attribute assigned through its accessor is itemized

A `$`-sigil attribute is a Scalar container, so an Array or Hash stored into
it renders with the itemizing `$` prefix and stops flattening under the
single-argument rule. Through the accessor, mutsu did neither:

```raku
class Foo { has $.x is rw }
my $f = Foo.new;
$f.x = {a => 1, b => 2};
say $f.x.raku;          # mutsu: {:a(1), :b(2)}   raku: ${:a(1), :b(2)}
```

The same literal assigned to a plain lexical (`my $x = {a => 1, b => 2}`) was
already right on both interpreters, so the divergence was specific to the
accessor store.

## Root cause

The ordinary local-slot store runs every `$` assignment through
`itemize_scalar_store` (`src/vm/vm_run_loop.rs`). The accessor store in
`assign_method_lvalue_with_values`
(`src/runtime/methods_mut_method_lvalue.rs`) committed `assigned_value`
directly, with no itemization step — in both of its `$`-sigil branches, the
generated accessor and the hand-written `is rw` method that exposes the same
attribute.

A second store through an `is rw` method was wrong for a further reason. It
took the in-place `store_into_attr_container` shortcut, which empties and
refills the destination container instead of replacing it. That shortcut is an
`@`/`%` rule — it exists so `method items { @!items }` keeps its container's
identity under concurrent writes (ADR-0068) — but nothing restricted it to
those sigils, so a `$` attribute took it too. For a Scalar that is the wrong
semantics outright (`$obj.w = {b => 2}` *rebinds* the scalar to the new Hash;
it does not mutate the Hash that was there), and it also discarded the
itemization the store had just applied, because the destination container kept
its own older tag.

## Fix

`itemize_attr_store_value` joins `check_attr_store_type` and
`attr_store_nil_default` as a rule both spellings of the accessor apply: it
itemizes for a `$` attribute and is the identity for `@`/`%`, which are
containers in their own right and must stay un-itemized. Both `$`-sigil
branches call it just before committing.

The `store_into_attr_container` shortcut is now guarded to `@`/`%`, matching
what the code around it already said — "only a *scalar* `$!attr` needs
`is rw` to expose the container for rebinding". A `%` accessor store still
refills its container in place, so the identity guarantee that motivated the
shortcut is untouched.

Itemization is not cosmetic. `my @flat = $obj.x` on a `$`-held three-element
Array now contributes one element, as rakudo does, where mutsu spread it into
three.

Pinned by `t/oo/attribute/scalar-accessor-assignment-itemizes.t`, whose
sixteen assertions all pass unchanged under rakudo: Hash and Array through the
generated accessor, through `$.x = ...` inside a method and through an `is rw`
method; second stores through both; the rebind-not-refill distinction observed
through an alias; the single-argument rule as behaviour rather than as `.raku`
text; and negative controls for `@`/`%` attributes, for a `%` attribute's
in-place refill, and for plain scalar values.

## Not fixed here

Three neighbouring divergences turned up while measuring this one and are
filed separately rather than widened into this change:

- [#9040](https://github.com/tokuhirom/mutsu/issues/9040) — an attribute's
  declared *default* (`has $.w is rw = {a => 1}`) is not itemized at
  construction. That is the object-build store, not the accessor, and it is
  the one remaining site that does not apply `itemize_attr_store_value`.
- [#9041](https://github.com/tokuhirom/mutsu/issues/9041) — `$obj.w = %src`
  copies the Hash where rakudo shares its identity, so a later `%src<y> = 2`
  is invisible through the attribute.
- [#9042](https://github.com/tokuhirom/mutsu/issues/9042) — a `Seq` assigned
  through the accessor is coerced to an Array: `$[1, 2, 3]` for what rakudo
  renders `$((1, 2, 3).Seq)`. `Slip`, `Range` and a bare `List` are already
  right.
