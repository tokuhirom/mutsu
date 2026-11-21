# RakuAST: origins of a modified statement under a prefix

`try die "x" for 1` holds the modified statement `die "x" for 1` inside its
`StatementPrefix::Try`, and mutsu left that statement and its expression
without `.origin` (#15780). The statement now runs from after the keyword to
the end of its modifier, and an expression the parser kept no range for is
placed from a fragment parse of the text before the modifier. `do (1) for 1`
also no longer ends its statement before the modifier.
