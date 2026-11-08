# `.AST` of an anonymous optional parameter `$?`

`sub f($?) { }`, `sub f(Int $?) { }` and `sub f($? where { ... }) { }` leaked the parser's internal
name into the tree and into `.DEPARSE` (`$__ANON_OPTIONAL__?`). The parameter target is now the bare
`$` sigil, as in rakudo (#14845).
