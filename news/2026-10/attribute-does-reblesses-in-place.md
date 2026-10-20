# `$attr does R` reblesses the Attribute meta-object in place

Rakudo implements `$obj does R` by reblessing the object into the mixin type
`C+{R}`, so every holder of the object sees the role. mutsu already did this
for instances of declared classes, but an `Attribute` meta-object kept the
older `Mixin` wrapper, which only the variable that ran the `does` saw.

When a trait handler passed its `$attr` to another handler that mixed a role
in (`&trait_mod:<is>($attr, :mooish)` inside AttrX::Mooish's
`is custom-attr`), the calling handler's `$attr` therefore stayed a plain
`Attribute`. The next method call failed with `No such method 'base-name'`.

`Attribute` now takes the rebless path too:

- The `Attribute` dispatch shape covers the `Attribute+{...}` mixin types, so
  the reblessed object keeps its own native methods (`name`, `package`, `rw`,
  `get_value`, ...). The role's methods still win over them, through the
  user-override check.
- The code that recognized an attribute meta-object by the class name
  `Attribute` uses one predicate, `value::attribute_class`. That code includes
  the trait-target markers, `.WHY`, `is rw`/`is built` effects and the
  attribute-trait writeback.
- The attribute-trait machinery records a reblessed meta-object the same way
  it recorded a `Mixin`, and runs its `compose` hook.

Three general bugs this exposed are fixed along the way:

- A role stub such as `method abs {...}` is satisfied by a built-in parent's
  method (`class Y is Int does S`). Before, the stub check rejected the
  class, or the stub shadowed the native method.
- The roles a mixin type composes rank one step below the object, next to its
  base class. Before, they ranked after `Mu`, so `multi f(CX $, $)` lost to
  `multi f(P $, Mu $)` for a `P` that `does CX`.
- A role method called on a mixin-type object (`C+{M::R}`) runs in the
  role's package `M::R`, as it does when the role is composed into a declared
  class. Before, the `::` inside the braces made the method run in the mixin
  type's own package. So a `multi sub` the method declares was registered
  where a `samewith` could not find it ("Unknown function", AttrX::Mooish's
  `set-option` once the attribute was reblessed).
