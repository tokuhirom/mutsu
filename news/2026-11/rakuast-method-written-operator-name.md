# RakuAST: a method keeps its written operator name

`method postcircumfix:«[ ]»(...)` and `method infix:<< + >>(...)` lost the quote form
they were written with in `.AST` and `.DEPARSE` (the name was viewed as the canonical
`postcircumfix:<[ ]>`), while the same forms on a `sub` already kept it. A method's
written name is now recorded like a sub's, so the tree and its deparse match rakudo
(`QuotedString` with `processors => <quotewords val>`, `postcircumfix:<<[ ]>>`).
