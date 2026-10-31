# Generic `::T` captures and core enum values for `static`'s analysis

For `static` 0.0.1 (#15610), measured against rakudo 2026.09: a `::T` type
capture's `compile-time-value` is the generic type `T` itself -- the same object
a signature returns with `--> T` -- and `T.^archetypes.generic` is true (the
parser notes capture names; `Metamodel::Archetypes` answers `generic`). A core
enum value (`True`, `Inf`) in the `$*CU` tree resolves to a setting constant
whose `compile-time-value` is the value.

`static` 0.0.1 against rakudo (`not ok` before -> after): t/09 38 -> 26,
t/10 20 -> 0.
