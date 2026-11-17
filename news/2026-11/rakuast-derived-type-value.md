# A RakuAST derived type node knows its type at compile time

`RakuAST::Type::Definedness` (`Int:D`), `RakuAST::Type::Parameterized`
(`Array[Int]`, `Hash[Str,Int]`) and `RakuAST::Type::Coercion` (`Str(Int)`)
now answer `compile-time-value` and `maybe-compile-time-value` with the type
they name, as on rakudo 2026.09; before, both answered nothing. In the unit tree
`$*CU` the type names inside are resolved, so the type is built by evaluating
the node; a hand-built node whose names are not resolved dies on the first
unresolved one, in rakudo's order (a parameterization's arguments before its
base, a definedness's or coercion's base before its constraint).

`RakuAST::Term::Name.^can('maybe-compile-time-value')` is now true, as in
rakudo (the method answered already, through the node's shared row). Part of
#14916; the regression test is `t/rakuast/rakuast-derived-type-value.t`.
