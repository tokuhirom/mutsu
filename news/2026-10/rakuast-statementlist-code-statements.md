# `RakuAST::StatementList.code-statements`

`$list.code-statements` answers the unit's statements as a List. Rakudo
declares the method (undocumented) and `static::CLI`'s analysis reads it to find
`CATCH`/`CONTROL` handlers; on every statement kind measured under `raku`
(expressions, declarations, `use`, phasers, loops, `CATCH`, empty statements) it
answers the same nodes as `.statements`. Added under the standing policy for
`RakuAST::*` methods that rakudo has and a module needs (#15315).
