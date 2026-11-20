# RakuAST: `is-pure` and `short-circuit` of the operator nodes

`Infix`, `Prefix`, `Assignment` and the metaoperators answer `is-pure`
(false for `does`, `xx`, `~~`, `:=`, atomic assignment, `++`/`--` and assignment) and
`Infix` answers `short-circuit` (true for `&&`, `||`, `//`, `and`, `or`, `andthen`,
`orelse`, `notandthen`), matching rakudo 2026.11 over every built-in operator symbol
(#14916).
