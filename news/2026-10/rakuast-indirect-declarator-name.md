# RakuAST round-trips `::(EXPR)` declarator names

`sub ::("x") { }`, `method ::($n) { }` and `class ::(EXPR) { }` were refused by
the RakuAST conversion. They now render the name as Rakudo 2026.09 does,
`Name(Part::Empty, Part::Expression(EXPR))`, and lowering reads the expression
back into the declaration's `name_expr` (a literal or bare word also keeps its
text as the declared name, anything else the parser's placeholder). Three
more `t/` files pass under `MUTSU_RAKUAST=1`. ADR-10723 section 6.53.
