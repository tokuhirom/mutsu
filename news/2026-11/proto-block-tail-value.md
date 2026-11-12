# Return a proto declaration from block and routine tails

A `my proto` declaration at the end of a block or routine now yields its
dispatcher, matching a proto declaration used directly as an expression. The
returned `Sub` retains its signature instead of the tail yielding `Nil` or
`Any`.
