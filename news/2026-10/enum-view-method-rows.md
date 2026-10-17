# An enumeration's key/value views are method rows

`pairs`, `keys`, `values`, `kv`, `antipairs` and `invert` of an enum are now
`Any`'s method-table rows, the `Any(Enumeration)`, `Any(Bool)` and `Any:U`
candidates Rakudo declares (ADR-11276 §9.145). An enum value lists its whole
enumeration as in Rakudo: `red.pairs` was `(0 => red)` and `red.invert` died.
`True.invert` answers, and `Bool`'s views list `False` before `True`, like
`Bool.enums`. `Mu.set_why` is a row too, so `Mu.^can('set_why')` is true. Two
arms that answered methods Rakudo does not have (`Color.enum_value_list` and
`"Color".invert`) are gone.
