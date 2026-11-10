# EVAL($ast): `die` argument lists, `temp $obj.accessor<k> = v`, `$( sub ... )` scope

`EVAL(slurp($file).AST)` joined four more `t/` files to the ratchet. A block-body
`die "a", "b"` now reads back as two arguments (it was one comma expression) and
lowers to the same message-joining statement as the source; the statement-prefix
`try die "a", "b"` no longer drops the second argument. `temp $h.args<k> = v`,
whose container is whatever a method call yields, lowers through the same save
statement as the parser. The undeclared-routine check no longer treats `$( ... )`
(a desugared item-context group) as a lexical scope, so a `sub` declared inside it
is visible afterwards, as in rakudo.
