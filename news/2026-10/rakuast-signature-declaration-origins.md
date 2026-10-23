# RakuAST `my (...)` signature declarations carry their origins

`.AST` now gives a `my ($a, Int $b) = ...` declaration the ranges Rakudo
does: the `Signature` between the parentheses, each `Parameter` with its
type, target and the `VarDeclaration::Simple` the target visits, and the
`Initializer` (`=` or `:=`) of the declaration.

The parser desugars such a declaration into one declaration per variable, so
the signature is read back from the converted parameters and the written
text: every parameter must be exactly `[Type ]$name`, separated by commas.
A signature with an anonymous variable, a default, a trait or a
destructuring stays without ranges: exact or absent.

A routine's parameter now also gives the `VarDeclaration::Simple` under its
target the target's range.
