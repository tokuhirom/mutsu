# Smartmatching against `Nil` no longer accepts `''`, `[]` and `{}`

`'' ~~ Nil` (and `[] ~~ Nil`, `{} ~~ Nil`) was True: with `Nil` on the right-hand side the smartmatch fell through to the generic string comparison, where `Nil` stringifies to the empty string. A `given $v { when Nil { 'null' } when Str { ... } }` therefore sent every empty string to the Nil branch (BDD::Behave's JSON-events writer emitted `null` for an empty id). `Nil` on the right now accepts only Nil itself and a Failure.
