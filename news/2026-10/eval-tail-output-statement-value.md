# EVAL of a unit ending in `say`/`put`/`print`/`note` returns True

`EVAL q[say 1]` (and `put`, `print`, `note`, plus `Q[say 3].AST.EVAL`) now evaluates to `True`
like rakudo instead of `Nil`. The statement-form output builtins as the unit's tail leave
`True` as the result, as a routine body already did.
