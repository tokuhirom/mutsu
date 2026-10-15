# `Any`'s key/value views answer from the method table

The name-matching arms that answered `.keys`, `.values`, `.kv`, `.pairs`, `.antipairs`, `.pairup`, `.invert`, `.kxxv`, `.hash`,
`.sum`, `.permutations`, `.combinations` and `.cache` for receivers without a row of their own are gone (ADR-11276 §9.109). A
`Range`, an unsettled `Seq`, a shaped array, `Nil`, a stash, a type object and the other closed shapes now reach `Any`'s rows,
which carry the arms' bodies. Three answers moved to Rakudo's: `.kxxv` is `Baggy`'s alone (`set(<a>).kxxv` is
`X::Method::NotFound`), `.invert` of a non-`Pair` receiver (`"a".invert`, `(1..2).invert`) is `X::TypeCheck`, and `"x".IO.hash` is
the odd-initializer error.
