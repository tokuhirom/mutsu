# `.DEPARSE` of a bare subset and of a loose `xor`

`RakuAST::Type::Subset` now answers `.traits` when the subset has none
(`subset Size where { ... }`), as rakudo does: the vendored `RakuAST::Deparse`
iterates that list, so deparsing such a subset used to die with "No such method
'traits'" (it took all of `roast/integration/advent2012-day03.t` out of the
`.DEPARSE` corpus). The loose `xor` keeps its spelling in the tree
(`ApplyListInfix(Infix("xor"))`) and deparses as `xor` instead of `^^`, including
through the L10N deparsers; `EVAL` of the tree lowers it back to `^^`.

The pinned `.DEPARSE` corpus (#15792) moves from 2387 to 2395 of 2412 statements
identical to rakudo 2026.11 (99.3%).
