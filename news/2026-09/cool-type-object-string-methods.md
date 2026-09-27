# `Str.comb` refuses the type object instead of reading its gist

`Cool`'s string methods on an undefined `Cool` receiver answered out of the
type object's gist (#9772). `Str.comb` was `("(", "S", "t", "r", ")")`,
`Str.chars` was `5`, `Str.uc` was `"(STR)"`, and `substr($undefined, 0, 2)` was
`"(A"`.

Every candidate raku declares for these methods takes a defined invocant, so a
`Cool` type object (`Str`, `Int`, `Rat`, `List`, ...) now gets
`X::Multi::NoMatch` for `comb`, `lines`, `words`, `substr`, `index`, `trim`,
`ords` and the rest. The exception is `Str`'s own `:U` candidates for the
case-mapping and counting methods: `Str.uc`, `Str.flip`, `Str.chars` and
friends stringify the type object to `""` with the usual uninitialized-value
warning, as rakudo does.

The sub forms (`substr`, `uc`, `chars`, `index`, `lines`, `comb($matcher, $x)`,
...) are the method on their subject argument, so they now answer the same way.
That includes `X::Method::NotFound` for an undefined `Any`, whose method form
[`any_cool_method_gate`](../../src/runtime/any_cool_method_gate.rs) already
refused.
