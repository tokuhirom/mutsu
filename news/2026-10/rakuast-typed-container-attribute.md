# RakuAST: a typed `@` / `%` attribute is an empty container, not its type object

Under `MUTSU_RAKUAST=1` (ADR-10723, #7564), `has Int %.h` started as `Int` (its
type object) instead of an empty `Hash[Int]`, and `has Str:D @.e` rejected the
first element pushed into it. Lowering planted the implicit default of a typed
attribute (its type object) on every sigil, where the parser plants it only on a
scalar that is not `is required` -- and gives an `is box_target` attribute its
allocated body.

`attribute_type_seed` is that rule in one place, called by the parser's
attribute declaration and by `lower_attribute`. Nine `t/` files that ran but
behaved differently now pass (typed container attributes, `is box_target`
NativeCall attributes); `t/rakuast/rakuast-typed-container-attribute.t` pins the
rule.
