# Same-named enums in different packages are distinct types

Two packages that each declared an enum with the same short name used to share one enum
type: `registry.enum_types` was keyed by the bare declared name, so whichever declaration
ran last replaced the other, even when reached through its qualified spelling.
`module A { our enum pn <x y> }; module B { our enum pn <z> }` made `A::pn.enums` answer
`(z)`. CSS::Module declares `our enum prop-names` in both its CSS 2.1 and CSS 3 metadata
modules, so all three CSS::TagSet test files died with
`Cannot convert value to native integer type 'uint8'`.

A package-scoped enum is now registered under its package-qualified name, the same key a
nested class gets, and its values carry that identity (ADR-0128). Rakudo keeps displaying
such an enum under the name it was declared with (`A::pn.^name` is `pn`, `A::pn::x.raku`
is `pn::x`), so the declared name is recorded separately for the display layer only. An
enum in a class or role body resolves from the owner's methods like a nested class, an
enum in a role body registers under the role's package whoever composes it, and
`is export` now exports the enum's type name along with its values.

CSS::TagSet now gets past the enum lookup; its next failure is a separate one: assigning
a `Map` to a `%!attribute` keeps the immutable `Map`.
