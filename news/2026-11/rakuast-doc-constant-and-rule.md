# `.AST` keeps the `#|` doc of a `constant`, a `rule` and a `token`

A `#|` / `#=` comment on `constant K = 3`, or on a `rule` / `token` / `regex` written outside a
grammar, was dropped from the tree; it is now the declaration's `.declarator-docs(...)` and its
`Doc::Declarator` child, as in rakudo. Part of #14880. Still open there: `my \x = 3` and
`my ($a, $b)` (the parser records no doc site for those forms).
