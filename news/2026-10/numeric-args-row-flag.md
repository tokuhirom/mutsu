# A numeric parameter of a built-in row coerces a user object

The pre-dispatch rewrite of every `log`/`exp`/`atan2` call's object arguments is gone (ADR-11276
§9.184). The arity-1 `log`/`exp`/`atan2` rows carry a new `RowFlags::NUMERIC_ARGS`, and the
entry for calls whose arguments the guard step declines coerces a user object (through `Bridge`
for a `Real` class, `Numeric` otherwise) before handing the row the number, as Rakudo's
`Num(Real) $base` parameter does.
