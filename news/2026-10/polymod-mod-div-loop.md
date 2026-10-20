# `Real.polymod` decomposes with `mod` and `div`

`Int.polymod` with a non-`Int` divisor no longer uses its own exact-rational or
floating-point loop. Each step is Rakudo's `$rem = $n mod $d; $n = $n div $d`
on the shared operators, so `10.polymod(2.5)` is `(-2.5, 5)`. A `Num` invocant
decomposes in floating point and keeps a `Num` quotient (`2.5e0.polymod(2)` is
`(0.5e0, 1e0)`), a zero element is always an `Int`, a zero divisor throws
`X::Numeric::DivideByZero` and a negative invocant is an `X::OutOfRange`
failure. `Rat` and `Int`-over-`Int` receivers keep their exact paths.
