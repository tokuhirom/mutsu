# `$*CU` resolves the types a unit's slangs minted before its check code runs

A module that read `$*CU` while it was imported (Qwiratry's `BodyRewrite`, for
`aggreduce` bodies) got a tree built before the unit's own `aggsub Tally` ran,
so `Tally.({ $_ })` was an unresolved call and the CHECK-time rewrite left the
body alone (`aggregate-aggsub`: "Too many positionals passed"). The tree is now
built again just before the unit's check code runs, so a class a slang
production minted and bound as a constant is a `Type::Simple` with a
compile-time value, as in Rakudo (#13370).
