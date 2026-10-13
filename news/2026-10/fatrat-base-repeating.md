# `FatRat.base-repeating`

`base-repeating` is declared on `Rational`, so a `FatRat` now has it too, also when its numerator or denominator
is a big integer (#13258). `Rat` and `FatRat` share one implementation over big integers, and an integral value
renders as its decimal `Str` in every radix, as Rakudo's does (`(10/1).base-repeating(16)` is `(10 )`, not `(A )`).
