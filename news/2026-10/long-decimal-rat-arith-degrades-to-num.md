# Arithmetic on a long decimal Rat degrades to Num, not FatRat

A decimal literal such as `0.1234567890123456789012345` (or `Str.Numeric` of
one) is an exact `Rat` whose denominator does not fit in 64 bits. Arithmetic on
it used to return a `FatRat`, silently switching the program to arbitrary
precision. The arithmetic paths classified any big-denominator rational as
"FatRat-like" through a fallback that assumed only FatRat operations can build
one — wrong for literals. The FatRat flag is now the sole authority, so
`$r + 0`, `$r * 1`, `$r + 0.5` and friends degrade to `Num` as in Rakudo, while
real `FatRat`s and `$*RAT-OVERFLOW = FatRat` keep upgrading (#11428).
