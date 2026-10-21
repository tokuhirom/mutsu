# Multi ranking: a mixed-in role no longer out-ranks an unrelated nominal type

`multi f(R $a, Numeric $b)` used to beat `multi f(Rational:D $a, Rational:D $b)`
for `f(1.5 but R, 2.5)`, because the role mixed into the argument is nearer
than the value's own type. Rakudo compares the declared types parameter by
parameter, where `R` and `Rational` tie and `Rational` is narrower than
`Numeric`, so the second candidate wins. `MultiRank::narrower_by_rank` now lets
the declared-type comparison veto a distance-only lead, and the bare builtin
roles `Rational`/`Real` know their `Real`/`Numeric` ancestors for it
(`builtin_role_ancestors`). Found while running CSS::Units (#14161).
