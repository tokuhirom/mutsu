# RakuAST: assignment to a parenthesised lvalue expression

`(COND ?? $a !! $b) = v`, `($a || $b) = v` and `nqp::op(..) = v` now cross the
RakuAST boundary as a plain `ApplyInfix(Assignment)` over a
`Circumfix::Parentheses`, matching rakudo 2026.09. `lower` hands the left side
back to the parser's own callable-lvalue writeback. Four more `t/` files pass
under `MUTSU_RAKUAST=1`. Part of S10 of #7564.
