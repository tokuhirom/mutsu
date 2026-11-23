# `1 [&plus] 2` is an `ApplyInfix` over a `FunctionInfix`

A routine used as an infix viewed as a call of the routine
(`ApplyPostfix(Var::Lexical("&plus"), Call::Term(1, 2))`) where rakudo keeps
the operator application, `ApplyInfix(left, FunctionInfix(Var::Lexical
("&plus")), right)`. The parser still calls the routine with both operands but
now marks the call (`Spelling::FunctionInfix`), the view builds rakudo's node,
and lowering turns the node back into the call, so `.DEPARSE` writes
`1 [&plus] 2` and `EVAL` runs it (#18469).
