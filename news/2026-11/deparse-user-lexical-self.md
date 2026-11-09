# A user lexical `$self` deparses and lowers as `$self`

The parser keys a user lexical (or parameter) spelled `$self` as `"$self"` so it cannot collide
with the invocant (ADR-0061). `.AST` / `.DEPARSE` printed that key's own sigil again (`$$self`),
and `EVAL($ast)` bound the plain invocant name instead. The node now carries the written name
`self` and lowers back to the keyed lexical.
