# `.^methods` lists the Rakudo-declared methods mutsu serves on Any, Instant, Duration and more

147 more catalog rows (Rakudo declares them directly on the owner and mutsu already answers them
through the interpreter): `Any`'s `all`/`any`/`one`/`none`/`cache`/`duckmap`/`flatmap`/`toggle`/...,
the `Real` math methods on `Instant` and `Duration`, `Range`/`Seq`/`Mix*`/`Bag*`/`Set*` rows,
`Exception.die`/`fail`/`rethrow`, `Date`/`DateTime`/`Rat`/`Num`/`Int` `new`, ... They are
`SPECIAL` rows (the pure arity cascade is not claimed to serve them), so only `.^can` and `.^methods`
change: `Any.^methods` 52 -> 65, `Instant` 45 -> 64, `Duration` 41 -> 60, `Range` 36 -> 47 (part of #13494).
`Mu` and `Junction` rows, and `of`/`keyof` (already listed through the composed role) were left out.
