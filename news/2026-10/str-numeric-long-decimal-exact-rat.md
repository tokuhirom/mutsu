# A long decimal string numifies to the exact Rat

Math::Root checks a 20 076-digit square root of 2 against the published
digits: `root(2, 2, 20076) == sqrt2()`, where `sqrt2()` returns the digits as
a Str. mutsu answered False.

`Str.Numeric` parsed a decimal with too many digits for an `i64` into an f64.
Rakudo keeps it exact: `"0.1234567890123456789012345".Numeric` is a Rat with
denominator 2e24, the same value the numeric literal of that spelling gives.
`Str.Numeric` now builds that exact Rat as well.

The `==` and `<=>` operators had their own copy of string numification.
`<=>` even parsed strings as Rust floats, so `"0x10"` and `"1/3"` were not
numbers to it. Both now numify a Str operand exactly as `.Numeric` does, so
`0.1234567890123456789012345 == "0.1234567890123456789012345"` is True and a
radix or fraction string compares at its value.

All three of Math::Root's test files pass. Arithmetic on such a Rat still
yields a FatRat where Rakudo yields a Num (#11428).
