# RakuAST: `$.name += EXPR` statements

A compound assignment to a public attribute written as a statement
(`method bump { $.n += 100 }`) no longer stops `.AST` with an internal
`__ANON_STATE__` name. It renders as the `ApplyInfix` over a
`Var::Attribute::Public` and a `MetaInfix::Assign` that rakudo 2026.09 builds,
the same node the parenthesized expression form already had, and four more
`t/` files pass under `MUTSU_RAKUAST=1`.
