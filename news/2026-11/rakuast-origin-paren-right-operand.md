# RakuAST: origins of a parenthesized right operand of an assignment

In `10 R+= ($a ||= 42)` the parser keeps no range for the parenthesized
right operand, so the `Circumfix::Parentheses` and everything inside it had
no `.origin` (#15780). Like an unplaced infix or postfix application there,
the parentheses now run from after the operator to the end of the
application, when their closing `)` ends it, and the inside is placed from
them.
