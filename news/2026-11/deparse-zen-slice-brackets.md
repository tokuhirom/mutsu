# A zen slice keeps its brackets in RakuAST and `.DEPARSE`

`@a<>`, `@a{}` and `*{}` all parse to the same decontainerizing zen slice, so `.AST` showed an
`ArrayIndex` and `.DEPARSE` printed `@a[]` (and lost `%h<>` altogether). The parser now records
which brackets a zen slice was written with (`Spelling::ZenBrackets`), and the view builds rakudo's
node for each: `HashIndex` over an empty `SemiList` for `{}`, `LiteralHashIndex` over an empty word
quote for `<>`. Lowering turns an empty `LiteralHashIndex` back into the zen slice, so
`EVAL($ast)` is unchanged.
