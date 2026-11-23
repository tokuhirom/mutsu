# `.AST` reads `our \T := Int` and `state \x = 1`

`Str.AST` refused a sigilless declaration with a scope other than `my`
("does not yet support this construct: scoped sigilless declaration"). It is
now rakudo's `VarDeclaration::Term` with a leading `scope` field (`"our"` /
`"state"`), `.DEPARSE` writes the scope back, and lowering the node gives the
scoped declaration again, so `EVAL` of the tree declares it as the source does
(#18426).
