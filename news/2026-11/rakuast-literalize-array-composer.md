# RakuAST: `literalize` of an array composer follows rakudo's rule

`literalize` of `[ EXPR ]` is now the literal of `EXPR`, and a comma list is the `List` of its
literalized operands: `[1]` is `1`, `[[1], 2]` is `(1, 2)`, `[{a => 1}]` is the `Map`, `[]` an empty
`List`. Before, an array composer always answered a `List`, keeping one-element wrappers
(`(1,)`) and nested arrays. Part of #16529; `{ 1, 2 }` as a hash composer is still a block here.
