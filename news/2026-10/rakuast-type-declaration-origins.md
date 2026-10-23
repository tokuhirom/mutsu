# RakuAST enum and subset declarations carry their origin

`.AST` now gives an `enum` or `subset` declaration's `Name` the source range
Rakudo does, and a subset's `Trait::Of` (`of Str:D`, its type down to the
`Name`) and `Trait::Is` (`is export`) theirs. The declaration itself already
had its statement's range.

The parser records the positions as it reads the declaration, in every parse,
in a `spans` member of `Stmt::EnumDecl` / `Stmt::SubsetDecl` (ADR-10723 §4).
The two implicit `Trait::Does` nodes Rakudo gives an enum, which the source
does not write, have no origin there either and are not built by mutsu.
