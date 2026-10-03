# Numeric infix routines no longer spread a lone list argument

`infix:<+>($[1,2])` answered 3 because `call_infix_routine` flattened any single
array argument. Rakudo's `+`, `-`, `*`, `/`, `**`, `%`, `gcd` and `lcm` have only a
`(\a)` one-arg candidate, so the array is numified (its element count, 2). Those
operators now skip the one-arg flattening; `infix:<~>` and the rest keep it.
Closes #11422.
