# RakuAST: `locate-node(:key)`

`$ast.locate-node($pos, $to, :key)` answers the nearest statement-like node
around the deepest node at the position: a `Statement::*`, a `StatementList`, a
`Blockoid`, a `Sub` or a `Method` (rule measured on rakudo 2026.09 over a
program of classes, subs, loops, regexes and blocks). Without `:key` nothing
changes (#14916).
