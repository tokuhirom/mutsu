# Nil assigned through a sigilless name resets the container default

`q = Nil` where `q` is a sigilless name bound to a shared Scalar cell (a
`for ($z,) -> \q` loop parameter) used to store a literal `Nil` into the
variable. The `SigillessAggregateStore` op now resolves the aliased container's
default (its `of` type object, else `Any`) before the store, matching Rakudo.
The typed hash entry bound with `:=` (`%h<k> := $y`) is tracked separately.
