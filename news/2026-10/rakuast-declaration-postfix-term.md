# RakuAST: a declaration with a postfix in expression position

`is my $k.defined, False, '...'` (a declaration followed by a postfix call as a
call argument, `roast/S03-operators/increment.t`) used to refuse under
`MUTSU_RAKUAST=1`: the parser wraps the declaration and its loose tail in a
`DoStmt` of a scopeless block, and only the statement-level conversion knew
that split. Expression position now reuses the same loose-tail conversion, so
the term renders as `ApplyPostfix(operand => VarDeclaration::Simple, ...)`,
identical to rakudo's gist. `roast/S03-operators/increment.t` passes under the
RakuAST frontend.
