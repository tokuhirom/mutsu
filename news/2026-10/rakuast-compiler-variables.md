# RakuAST: compiler variables are `Var::Compiler` nodes

`$?PACKAGE`, `$?LINE`, `$?FILE`, `$?DISTRIBUTION` and the other `$?` variables now
appear in a `.AST` tree the way rakudo 2026.09 builds them:
`Var::Compiler::Line(N)` and `Var::Compiler::File("path")` for the two the parse
resolves, a bare `Var::Compiler::Distribution`, and `Var::Compiler::Lookup("$?NAME")`
for every other name. mutsu used to show `$?LINE`/`$?FILE`/`$?TABSTOP` as the
literal the parser folds them into, and the others as plain variables.

A `.AST` parse keeps the written name beside the folded literal, so ordinary
parses and the compiled code are unchanged; the nodes are constructible from Raku
and lower back through `EVAL`. Found by the roast-wide `.AST.raku` comparison
(#7564); closes #14887's compiler-variable half.
