# Mix weights keep a big Int, BigRat or FatRat type

`MixWeight` gained a boxed `Big` variant, so a Mix/MixHash weight with no 64-bit
form is no longer degraded to a `Num`: `(a => 2**70).Mix<a>` is an `Int`,
`(a => FatRat.new(1, 3)).Mix<a>` a `FatRat`, and `$mh<a> = 2**70` stores the big
integer instead of the truthiness `1`. Weight addition falls back to the numeric
tower on `Value`s for a big operand or a total beyond 64 bits, so sums stay exact.
`MixWeight` is no longer `Copy`.
