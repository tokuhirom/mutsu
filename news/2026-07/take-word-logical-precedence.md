# `take` now binds tighter than loose word-logical operators

`take X and Y` now parses and executes as `(take X) and Y`, matching Raku's
operator precedence. The parser captures `X` in a unique internal value before
taking it, then uses that same value as the left operand of the word-logical
tail. This preserves short-circuit behavior without evaluating a side-effecting
`X` twice.

The same lowering covers the loose `andthen`, `notandthen`, `or`, `xor`, and
`orelse` families. Regression tests pin true, false, and undefined taken values,
including exact-once evaluation.
