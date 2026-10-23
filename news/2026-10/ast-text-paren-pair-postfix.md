# `.AST` drops the parentheses of a pair used as a postfix operand

`(a => 1).List` now renders `ApplyPostfix(operand => FatArrow ...)` like rakudo:
the one level of parentheses rakudo drops around a postfix operand was already
dropped for every expression except a positional pair, whose parentheses live
inside the `PositionalPair` marker. `((a => 1)).List` still keeps the inner level.
