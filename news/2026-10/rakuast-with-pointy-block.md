# RakuAST: `with` / `without` blocks with an explicit signature

`with X -> Int $v { … }` and `without X -> $z { … }` now cross the RakuAST
boundary as a `Statement::With` / `Statement::Without` whose clause is a
`PointyBlock`, matching rakudo 2026.09. The parser records the written
parameter and body at the head of the then-branch (`SourceForm::WithPointy`, which
the compiler skips); `lower` rebuilds the parameter binds through the same
`with_then_branch` the parser uses. A `with` block is also accepted in
expression position. Eight more `t/` files pass under `MUTSU_RAKUAST=1`. The
`orwith -> PARAM` clause is still refused (the parser drops its type).
Part of S10 of #7564.
