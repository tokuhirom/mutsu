# A declared module is a type term even when a same-named `sub` exists

`module foo {}` now registers `foo` as a type name in the parser, as `class` and
`role` already did, so a later `sub foo` no longer turns the bare term `foo`
into a call (`foo.HOW` is the `ModuleHOW`). `foo()` with an empty argument list,
for a name that is both a declared type and a declared sub, is the type's
coercion type `foo(Any)` instead of a call; the sub stays reachable as
`&foo()`. This fixes tests 147 and 148 of `S02-names-vars/names.t` (#15516).
