# RakuAST keeps the shape of an assignment to a non-variable lvalue

`-$x = 5`, `$<c> = $0` (also inside a regex code block) and an unparenthesized
`$y ?? 1 !! $x = 2` no longer render a spurious `Call::Term` or ternary
parentheses, and `($x) = 1` / `(@a) = 1` keep their `Circumfix::Parentheses` as a
list assignment (no `:item`), as in rakudo 2026.09. `(COND ?? a !! b) = v` keeps
the parentheses the source wrote. The parser tags the call-free lvalue
assignments (`Spelling::PlainLvalueAssign`, `GroupedTargetAssign`), a regex code
block is parsed with the unit's spelling setting, and EVAL lowers the hand-built
shapes back to the same execution.

Closes #14911.
