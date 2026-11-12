# `[~] ($x,)` on a captured variable keeps a Blob a Blob

The one-operand `[~]` reduction chooses its `infix:<~>` candidate by the lone
operand: a Positional is joined, a Blob is returned unchanged, anything else is
stringified. When the operand was a lexical captured by a closure
(`-> { [~] ($x,) }`), it reached the reduction as a reference to the variable's
shared container, so a Blob was stringified (dying with "Stringification of a
Blob[uint8] is not done with 'Stringy'") and an itemized List was rendered
`"1 2"` instead of joined to `"12"`. The candidate is now chosen by the value
the container holds. Found running RSV, whose `to-rsv` folds a module-constant
Blob inside a pointy block.
