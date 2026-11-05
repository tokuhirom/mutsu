# `round($rat)` is exact and `.narrow` narrows a big unit-quotient rational

The routine form `round($x)` rounded a `Rat` through an `f64`, so a value past 2**53 lost its low
digits (`round(11920928955078125.0)` answered `...124`) while `.round` was exact. It now calls the
one implementation `.round` uses. `.narrow` left a big-numerator rational whose quotient is a whole
number (`(5**33 / 5).narrow`, a big `FatRat`) a `Rat`; it now narrows to `Int` like the word-sized
case. Rat::Precise's `.precise` depends on both: its FatRat suite now matches rakudo.
