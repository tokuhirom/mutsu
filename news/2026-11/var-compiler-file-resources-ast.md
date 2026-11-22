# `$?FILE` and `%?RESOURCES` get their own RakuAST nodes

In a string's `.AST`, `$?FILE` was a generic `Var::Compiler::Lookup("$?FILE")`;
rakudo resolves it at parse time to `Var::Compiler::File("EVAL_N")`, the name
of the string's own compilation unit. A `Str.AST` parse now publishes that unit
name as the file `$?FILE` folds to, so the node, its `.DEPARSE` and its `EVAL`
match rakudo. `%?RESOURCES` was a plain `Var::Lexical("%?RESOURCES")`; it is now
rakudo's bare `Var::Compiler::Resources` and lowers back to the same variable
(#18689).
