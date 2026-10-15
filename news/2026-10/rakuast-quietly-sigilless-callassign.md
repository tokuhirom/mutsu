`quietly my \z .= Numeric` now round-trips through RakuAST. The frontend
preserves Rakudo's `StatementPrefix::Quietly` and `VarDeclaration::Term` with a
call-assignment initializer, then lowers it back to the parser's readonly
sigilless declaration shape.
