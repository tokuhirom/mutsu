# `@!attr := Proxy` / `%!attr := Proxy` seat the Proxy as the attribute

Binding a `Proxy` to an `@` or `%` attribute failed (`expected Positional but got
Proxy`) or turned the Proxy into a Hash of its parts, so AttrX::Mooish's
`AttrProxy` never became the attribute of an array or hash attribute and its
triggers, filters and lazy builders did not run. The bind check now looks at the
Proxy's FETCHed value (as Rakudo does: a Proxy answering an Array binds to `@`,
one answering `5` raises `X::TypeCheck::Binding`) and the Proxy itself is seated
in the attribute.

`Attribute.get_value` now hands out the attribute's cell (#14388), so a later
`nqp::bindattr` of a Proxy stored it INTO the cell instead of replacing the slot,
and AttrX::Mooish's non-lazy `:filter` stopped filtering. A bind of a Proxy or a
cell now replaces the slot (#14266). AttrX::Mooish 1.0.10: `t/011-arrays`,
`014-private`, `015-triggers`, `016-filtering`, `030-inheritance`,
`050-multi-filter` and `100-clone` pass in full.
