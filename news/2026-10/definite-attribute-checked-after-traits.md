# `has Int:D $.x` is checked for an initializer after the attribute's traits ran

Rakudo raises `X::Syntax::Variable::MissingInitializer` for a definite-typed attribute with no
initializer only after the attribute's traits have been applied, because a trait may mark the
attribute required. AttrX::Mooish does exactly that for `:lazy` attributes
(`nqp::bindattr($attr, Attribute, '$!required', 1)`), so `has Int:D $.x is mooish(:lazy)` is legal.

mutsu raised the error while parsing. For an attribute with a custom trait the parser now defers
the check; class registration repeats it once the traits have run, asking the trait-time
Attribute object's `.required`. An attribute without a custom trait is still rejected at parse
time, as before.

Mooish's own test (`self.type.HOW.archetypes.definite`) also needs the type seen by a trait to be
the definite type object: inside a trait handler, `$attr.type` of `has Int:D $.x` is now `Int:D`
(a `DefiniteHOW`), not `Int`.

Pinned in `t/oo/attribute/definite-attribute-checked-after-traits.t` (it passes under `raku` too).
On AttrX::Mooish (#13264), `t/013-validations` and `t/100-clone` now compile.
