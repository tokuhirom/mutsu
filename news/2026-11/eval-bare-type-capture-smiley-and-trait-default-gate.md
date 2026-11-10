# EVAL($ast): `Any:U ::T` keeps its smiley; a unit's `:default(` re-dispatch gets its prelude

`sub f(Any:U ::T)` (a type capture with no variable) lost its nominal type in `.AST`
(rakudo has a `Type::Definedness` over `Any`), so an `EVAL`'d `multi` pair
`(Any:U ::T)` / `(&c)` picked the `:U` candidate for a code argument. The capture
parameter now carries its nominal type beside `type-captures`.

The `trait_mod:<is>(Attribute, :$default!)` prelude gate asks for the `:default(`
spelling, which a unit with no source text (a string held in a variable, or a
tree) could not give. The unit's text view now spells a `trait_mod:<is>` call
passing a `default` pair, so a custom attribute trait that re-dispatches to CORE's
`:default` works in every `EVAL`, not only when the literal sits in the main program.
