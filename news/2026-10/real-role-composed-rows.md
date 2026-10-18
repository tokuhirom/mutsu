# The `Real` role's methods are rows, and only `Real` classes are bridged

A user class that composes `Real` now gets exactly the methods Rakudo's `Real` and `Numeric`
roles declare, as composed-role rows that compute through the class's `Bridge` (ADR-11276
§9.179). The old heuristic re-sent any unanswered method to `.Numeric`/`.Bridge` of every class
that merely had such a method; it is gone. So `.Real`, `.Numeric`, `.conj` and a non-negative
`.abs` answer the invocant, `.isNaN` is `False`, `.is-prime` is not a `Real` method, `~` and
`eq` stringify through `Real.Str`, `?$x` asks `Numeric.Bool` (`self != 0`), and a class with
only a `Bridge` method keeps `Mu`'s answers, as in Rakudo.
