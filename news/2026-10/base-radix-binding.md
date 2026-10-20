# `.base` binds its radix as each type's signature does

`.base`'s radix argument is now bound the way Rakudo's candidates bind it.
`Int.base` takes `Int(Cool) $base`, so a Str radix numifies (`10.base("2")`,
and `10.base("x")` raises `X::Str::Numeric`) and a fraction truncates.
`Rat.base` and `FatRat.base` (Rakudo's `Rational`) take only an `Int:D`, so
`3.5.base("2")` is the `X::Multi::NoMatch` listing their three candidates
instead of `11.1`. `Num`, `Instant` and `Duration` declare `Int:D $base`, so a
Str or fractional radix is an `X::TypeCheck::Binding::Parameter`. An `IntStr`
radix is an `Int` everywhere. `polymod` on a fractional receiver now numifies
a Str divisor to the number it spells (`3.5.polymod("2")` is `(1.5, 1.0)`) and
raises `X::Str::Numeric` for one that spells none (#14037).
