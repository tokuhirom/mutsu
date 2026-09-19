# A compose hook now sees the attribute's own role mixin via `.^attributes`

`apply_attribute_traits` stores a custom `trait_mod:<is>` handler's
`$attr does SomeRole` result into the registry's
`class_attribute_trait_objects` map so that `^attributes` later serves the
mixed-in meta-object instead of a fresh one -- but it only did so AFTER
calling any `compose` hook the same handler triggered on `$class.HOW`. A
`compose` method that reads `type.^attributes.grep(SomeRole)` right away --
exactly what `AttrX::Lazy`'s `LazyAttributeContainerHOW.compose` does to find
every `is lazy` attribute and install its accessor -- always saw the
pre-mixin object and found nothing, so the accessor was never installed.

The registry write now happens right after the attribute's own mixin value
is captured, before the compose hook runs, so `.^attributes` reflects it
immediately. This was root-caused while verifying #8815's second bug against
the real `Math::Matrix` + `AttrX::Lazy` ecosystem dependency: the issue's own
minimal repro (`type.^add_method` from a `$class.HOW does Role` compose hook,
with no attribute-mixin introspection) already passed on `main`, but
`AttrX::Lazy`'s actual `compose` method -- which additionally reads
`type.^attributes.grep(LazyAttribute)` to discover which attributes are lazy
-- did not.

Pinned by `t/oo/attribute/attribute-trait-mixin-visible-to-compose-hook.t`.

Part of #8815 (bug 2 of 2). A further, separate gap remains before the full
`AttrX::Lazy`/`Math::Matrix` pipeline works end to end: `.^method_table`
appears to include a public attribute's auto-generated accessor (so
`AttrX::Lazy`'s own conflict check false-positives), and `.^private_method_table`
is not implemented at all. Filed separately since neither is part of #8815's
two reported bugs.
