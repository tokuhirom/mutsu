# A declared `prefix:<not>` keeps `not`'s loose-unary precedence

The first version of the declared/imported `prefix:<not>` support parsed `not`
as a tight prefix operator, so with Red loaded `grep(not *.name in <b c>)` became
`(not *.name) in <b c>` and every "not in" assertion of Red's t/19-in-sql
failed. A candidate added to `not` does not change its precedence: the operand
is still the whole item expression (`not(*.name in <b c>)`), and only the node
differs -- a call of the declared `prefix:<not>`, with the builtin as fallback.
