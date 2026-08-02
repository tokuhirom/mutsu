# `does` mutates the object instead of copying it

Raku's `does` mixes a role into *the object*, so every reference to it sees the
mixin; `but` is the copying one. mutsu had it the other way round: `does` built a
fresh `ValueRepr::Mixin` wrapper and rebound the left-hand variable, so any other
reference to the same object was unaffected.

```raku
role Marker { }
class C { }
my $x = C.new;
my $y = $x;
$y does Marker;
say $y ~~ Marker;   # raku: True    mutsu (before): True
say $x ~~ Marker;   # raku: True    mutsu (before): False
```

Passing through a routine lost the mixin entirely, because the callee rebound
only its own parameter — which is exactly how every custom parameter trait in
`Cro::HTTP::Router` is written:

```raku
multi trait_mod:<is>(Parameter:D $param, :$query! --> Nil) is export {
    $param does Cro::HTTP::Router::Query;
}
```

So this was the root blocker under
[`todo/tickets/parameter-objects-have-no-stable-identity.md`](../../todo/tickets/parameter-objects-have-no-stable-identity.md),
and therefore under Cro::HTTP's router.

## The mechanism

Rakudo implements `$obj does R` by creating the type `C+{R}` — a class that
inherits from `C` and composes `R` — and *reblessing* the object into it. mutsu
now does the same for `Instance` values:

- `InstanceAttrs.class_name` became an interior-mutable `AtomicU32`, so an
  object's type can be retagged through the `Gc` node every alias already
  shares (`InstanceAttrs::rebless`). The old `with_class` node-fork stays for
  `instance_sharing_cell`.
- `Interpreter::ensure_mixin_class` registers `C+{R}` on demand through the
  ordinary `register_class_decl` path, with the role in both `parents` and
  `does_parents`. Each `does` stacks a new type on the previous one
  (`C+{A}+{B}`, exactly how Rakudo names it) rather than re-composing every role
  side by side — successive mixins of roles that declare the same method are
  legal (the later one wins), whereas a single composition of both would be
  `X::Role::Composition::Conflict`.
- `does_rebless_instance` seeds the composed roles' own attributes on the
  already-constructed object and runs their `BUILD`/`TWEAK` submethods directly
  (a submethod is not inherited, so it cannot be reached by an ordinary method
  call on the mixin type).

Because the object is now an ordinary instance of a real class, everything that
used to need the wrapper — `~~`, `.^name`, `.^roles`, `.^parents`, method
dispatch, `.raku` — works through the normal class machinery, and `$x === $y`
stays True across the mixin.

Values with no shared node of their own keep the wrapper. An `Int` or a `Str`
has nowhere to store a class tag; so do the internal descriptor objects whose
consumers read the composed result's mixin map (`.HOW` meta-objects, the
`Attribute.container` descriptor), a class that cannot be named as a declaration
parent (`Attribute`), and a parameterised mixin (`$o does R[Int]`, `$o does
R(5)`), whose per-object type arguments still live in the wrapper. `but` is
unchanged — it still copies.

## A duplicate found on the way

mutsu keeps a composed role in the class's MRO (Rakudo does not) while also
copying the role's methods into the class. A walk over every MRO level therefore
found the same method twice, so `$obj.*meth` / `$obj.+meth` called it twice and
the `nextsame`/`callsame` chain carried a redundant link — visible for a declared
`class D does R` too, not only for a mixin.
`drop_flattened_role_duplicates` now discards the role's own copy whenever a
class level already carries the flattened one.

## What is left

Materialized `Parameter` objects still have no stable identity: `.signature.params`
rebuilds them on every access. The parameter-trait consumer is handled on top of
this in [`param-trait-mixin-persists.md`](param-trait-mixin-persists.md); the
identity problem itself stays open in
[`todo/tickets/parameter-objects-have-no-stable-identity.md`](../../todo/tickets/parameter-objects-have-no-stable-identity.md).

Pinned by `t/does-mutates-the-object.t` (checked against raku).
