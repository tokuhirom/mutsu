# Collation's methods are rows of the method table

`$*COLLATION`'s `primary`, `secondary`, `tertiary`, `quaternary`, `set`, `gist` and `raku` are now rows of
`Collation`, found by their owner (ADR-11276 §9.131), instead of one name `match` and a special case of the mutating
method call. `Collation.collation-level` exists now, and a `.set` through any alias of the collation (a variable, an
array element) is seen by all of them, because it writes the object's shared state.
