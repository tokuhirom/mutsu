# A declined method call walks the receiver's MRO for a row

`contains`, `index`, `rindex`, `starts-with`, `ends-with`, `substr`, `substr-eq`, `comb` and `wordcase` of `Str` and
`Cool` are now rows that take every argument form (a regex needle, `:i`, a position, a range, `:filter`), and the
fifteen text methods the interpreter answered by name are gone (ADR-11276 §9.129). A call no receiver-specific answer
took now walks the receiver's MRO, as `.^can` sees it, for the first row that binds it: `(1..5).contains(/3/)` is
`Cool`'s, a `Cool` instance with its own `Str` is read through it, and `"".subst` raises `X::Multi::NoMatch`. As in
Rakudo, a receiver that is not `Cool` (a plain object, a `Pair`, a `Version`, a `Capture`) no longer answers these
methods from its string form.
