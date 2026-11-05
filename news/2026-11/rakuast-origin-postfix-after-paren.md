# RakuAST: origins after a parenthesised operand

In `($i)++` and `(@a)[0]` the parentheses are not a node of their own in rakudo's tree, so the
operand ends before their `)`. The `Postfix` and the subscript (`ArrayIndex` with its `SemiList`)
are now placed after that `)`, as rakudo places them (#15780).
