# `RakuAST::Origin.as-match`

`$node.origin.as-match` answers a `RakuAST::Origin::Match` for the node's span:
`.file`, `.line` (the line the span begins on), `.from`, `.to`, `.orig` (the
unit text), `.Str` (the span's text) and `.chars`. Rakudo declares the method
(undocumented); `static::CLI`'s analysis reads `$match.file ~ ':' ~ $match.line`
for each node. The maintainer approved this method alone on #15597. `use
static::CLI` now gets past it and stops at the next undocumented method,
`RakuAST::StatementList.code-statements`.
