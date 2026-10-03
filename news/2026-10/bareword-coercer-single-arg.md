# Bareword coercers follow the single-argument rule

`List((1, 2))`, `Array((1, 2).Seq)` and `List($(1, 2))` wrapped their lone
argument as one element (`((1 2))`, `[(1 2)]`), and `Int((1, 2))` /
`Num((1, 2, 3))` answered `0`. A bareword coercer called with one argument now
coerces it like the method form — `List(x)` is `x.List`, `Int(x)` is `x.Int`,
so a List or Seq contributes its elements or numifies to its element count —
and returns a value already of the target type unchanged (`List([1, 2])` stays
the Array, `Int(True)` stays `True`). Several arguments still keep one element
each (`Array(1, (2, 3))` is `[1 (2 3)]`). The duplicate per-type `Int`/`Num`
arms in `builtin_coerce` are gone (#11299).
