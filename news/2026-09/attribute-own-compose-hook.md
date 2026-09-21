# An attribute's own `compose` hook now fires, not only a `$class.HOW` one

A custom attribute trait can install a lazy-accessor-style hook two ways in
real Rakudo: mix a role into the composing class's `.HOW` meta-object (the
AttrX::Lazy shape mutsu already supported via #8845), or mix a role directly
into the `Attribute` object itself (`$attr does Builder[$block]`, with no
`.HOW` involved at all). The second mechanism relies on a fact mutsu didn't
model: real Rakudo's `Attribute` type carries a native no-op
`compose(Mu $package)` method that every attribute is polymorphically
dispatched through during class composition, so a mixed-in override runs
automatically (verified against `raku`: `Attr.can('compose')` is already
true with no trait applied at all).

mutsu's `apply_attribute_traits` only ever queued a deferred `compose` call
for a mixin found on `$class.HOW`; an attribute-own mixin's `compose` method
was silently never invoked. The `Attribute::Lazy` ecosystem distribution
(zef; locked on tokuhirom/mutsu#7884) hits exactly this: its whole `will
lazy { ... }` trait is built on the attribute-own mechanism, and its
`t/020-trait.t` regressed with every assertion failing (the lazy block never
ran).

The fix generalizes the existing deferred-compose queue
(`PendingAttrCompose`, `run_pending_attr_composes`) to carry either target —
`How(owner)` as before, or the new `Attribute(owner, attr_name)` — and fires
`.compose($package)` on whichever object actually carries the hook. The
`classes_composing_accessors` gate that hides a public attribute's
auto-generated accessor from `.^method_table` during composition (#8836) is
now scoped to the `$class.HOW` path only: an attribute's own `compose` hook
reads `.^method_table` *after* its own `callsame` (verified against `raku`:
the accessor's visibility flips from `False` to `True` across that exact
`callsame`, within the same compose call), so ungating it there is what lets
`$package.^method_table{$meth-name}.wrap(...)` see a real `Method` object to
wrap instead of `Any`.

`Attribute::Lazy` 0.0.7 moves from `partial` (2/3 baseline files, 3/8
assertions) to `green` (3/3 files, 8/8 assertions). Pinned by
`t/oo/attribute/attribute-own-compose-hook.t`.
