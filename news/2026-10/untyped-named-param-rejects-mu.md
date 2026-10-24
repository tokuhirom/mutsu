# An untyped named parameter no longer wins dispatch for a `Mu` type object

`multi f($v, :$type = ...)` next to `multi f($v, Mu :$type!)` picked the first candidate for
`f(1, :type(Mu))` and then died binding `:$type` ("expected Any but got Mu"). An untyped `$`
parameter is `Any`, which the positional matcher already enforced when ranking candidates;
the named one skipped that check. The candidate is now inapplicable, so the `Mu :$type!`
sibling is chosen, as in rakudo.

This is how Red's `ast-value :type(.type), ...` reaches its `Mu`-typed candidate for an
attribute whose type is `Mu` (Red `t/46-rw`, `t/86-prefetch`).

Refs #14094.
