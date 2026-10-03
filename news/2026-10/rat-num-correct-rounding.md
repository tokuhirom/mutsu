# Rat → Num rounds the exact ratio

A `Rat` or `FatRat` whose numerator or denominator is past 2**53 used to
numify by converting each part to a double and dividing. That rounded each
part on its own, so the literal `-39.969480000000004` (nude
`-9992370000000001 / 250000000000000`) became `-39.96948` in `.Num`, in mixed
`Rat`/`Num` arithmetic and in comparisons.

All the native-int `Rat`/`FatRat` → `Num` conversions now go through one
routine, `value::rat_to_f64`. It keeps the single IEEE division when both
parts are exact doubles. Otherwise it divides the exact ratio and returns
the nearest double, as Rakudo does. Geo::WellKnownBinary's test suite now
passes.
