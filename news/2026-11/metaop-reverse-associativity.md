# The `R` metaoperator reverses associativity

`R` reverses its base operator's associativity, so `R-` and `R/` are
right-associative. mutsu folded every chain of them to the left:
`1 R- 2 R- 3` gave `2` (rakudo `0`, `1 R- (2 R- 3)`), and `8 R/ 4 R/ 2` gave `4`
(rakudo `0.0625`). A reversed operator also binds the operand nearest to its
left at its level, so `1 + 2 R- 3` is `1 + (2 R- 3)` = `2` (mutsu gave `0`),
while `1 R- 2 + 3` still groups as `(1 R- 2) + 3`. The additive and
multiplicative levels now parse a run of `R` operators (plain or bracketed,
`[R-]`) as a right-nested chain, and the RakuAST view nests the same way (#18907).
