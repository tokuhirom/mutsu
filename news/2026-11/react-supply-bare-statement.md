# `react STATEMENT if COND` parses; `react`/`supply` hold their bare statement

`react say 1 if 1` died with "Missing block": the statement form of `react`
stopped before the modifier, which then started an `if` statement of its own.
The modifier is now part of the react's statement, as for every statement
prefix. In the RakuAST view, `react STATEMENT` and `supply STATEMENT` (including
`supply whenever ...`) hold the statement itself, as rakudo does, instead of a
plain call or the braced form, and `EVAL` of a `supply STATEMENT` node now
lowers (#18801).
