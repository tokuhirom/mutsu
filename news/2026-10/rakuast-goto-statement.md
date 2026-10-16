# RakuAST goto statement round-trips

RakuAST conversion now represents an internal `goto` statement as the bare
`goto` call used by Rakudo, preserving bare label targets as name arguments and
computed targets as expressions. Lowering maps the call back to the existing
`Stmt::Goto` execution path.
