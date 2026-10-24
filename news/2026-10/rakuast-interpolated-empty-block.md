# RakuAST: an empty `{ }` in a string is a Block

`"a {  } b"` and `"{}"` now parse the empty closure as a block, so `.AST` has the
`Block(Blockoid(StatementList))` segment rakudo gives instead of dropping it. As in rakudo,
interpolating it is `Nil` in string context (the usual warning). Row 5 of #14820.
