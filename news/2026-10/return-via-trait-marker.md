# `returns` / `of` no longer leave `__return_via_*` markers in `custom_traits`

A return type written as a `returns` / `of` trait pushed `__return_via_trait` / `__return_via_of`
into the routine's `custom_traits` so that (a) `EVAL`'s type scan could tell an undeclared
`returns Foo` (`X::InvalidType`) from an undeclared `--> Foo` (`X::Undeclared`) and (b) the RakuAST
converter could keep the two trait spellings apart. Both facts are already in the written-trait
record (`routine_trait::RoutineTrait::{Returns, Of}`), so the type scan asks that record
(`return_type_is_trait`) and the markers are gone from the parser, `lower` and `convert`.
