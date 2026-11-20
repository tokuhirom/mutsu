# `.AST`: a reduction's arguments and metaoperator infix, and `Term::Rand`

`[+] 1, 2` is now `Term::Reduce` with the list's elements as its arguments (not one
comma `ApplyListInfix`), and `[Z+]` / `[X+]` / `[R-]` carry their `MetaInfix::*` node as
the infix, as rakudo 2026.11 does; `EVAL` of that tree lowers back to the reduction.
`Term::Rand` renders as `RakuAST::Term::Rand.new` without the empty parentheses (#18252).
