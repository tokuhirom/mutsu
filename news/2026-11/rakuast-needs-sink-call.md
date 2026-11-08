# RakuAST `needs-sink-call` answers per class

`RakuAST::Node.needs-sink-call` now answers as rakudo 2026.09 does: `True` by
default; `False` for declarations and never-sunk nodes (packages, routines,
regex thunks, `Statement::Expression`, loops, `Ternary`, `VarDeclaration::*`,
`ApplyDottyInfix`, `Type::Enum`, `Statement::Require`); the `Int` `1` for a named
call; and for `ApplyInfix` the `Int` `0` (assignment, `:=`, `~~`, `does`,
meta-assignment, looking through `Hyper`/`Reverse`) or `1` (`xx`). The
answers were measured over ~160 node classes of parsed trees and over every
`RakuAST::` class, and a differential run shows no class where mutsu disagrees.
