# A refined multi candidate carries the bind-check flag

`multi f(:$a)` and `multi f(UInt $s = 1, :$a)` tie for `f()` in Rakudo (a `where` clause or
`subset` type counts as a bind check, and refinements are only compared on the positionals two
candidates share), so the first declared wins. mutsu ranked the refined one narrower and ran the
wrong candidate, which made Data::Generators' `random-string()` return a List. Refined candidates
now also skip the optional-positional step and are never reported ambiguous when they tie
(`multi g(UInt $x)` declared twice runs the first).
