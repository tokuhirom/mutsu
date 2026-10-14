# RakuAST: unresolved barewords round-trip

A bare word that neither the setting nor the unit's declarations resolve used
to stop `.AST` conversion. It now renders as an argument-less
`RakuAST::Call::Name` carrying a hidden marker, and lowering restores the
parser's `BareWord`, so the round trip evaluates exactly as the parsed program
does. The `MUTSU_RAKUAST=1` ratchet gains nine files (runtime-resolved class
names, indirect declarator names, term operators, undeclared-name tests).
