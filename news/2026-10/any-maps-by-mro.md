# flatmap, deepmap, duckmap and nodemap are rows of Any

`flatmap`, `deepmap`, `duckmap` and `nodemap` were answered only by name-matching arms in the interpreter's method
cascade. They are now rows of `Any`'s method table, found through the receiver's MRO like `map` and `grep`
(ADR-11276 §9.134). `.flatmap` with more than one mapper is `X::Multi::NoMatch`, as in Rakudo; before, the extra
mapper was silently dropped.
