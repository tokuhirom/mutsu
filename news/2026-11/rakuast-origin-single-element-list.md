# RakuAST: origins of a one-element list

A list written with a trailing comma and one element (`("a",)`, `[1,]`, `5,`) is an
`ApplyListInfix` whose range, its comma `Infix` and the `SemiList`/`Statement::Expression`
around it now carry origins, as rakudo's do (#15780). `list_origins::apply_list_infix` only
placed lists of two or more operands.
