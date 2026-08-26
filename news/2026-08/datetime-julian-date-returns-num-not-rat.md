# `DateTime.julian-date` / `.modified-julian-date` are exact `Rat`s again

`DateTime.new('2021-12-24T12:23:00.43Z').julian-date` returned the `Num`
`2459573.015977199` where rakudo returns the exact `Rat` `2459573.0159772`
(`<21250710858043/8640000>`); `.modified-julian-date` diverged the same way
(`59572.51597719907` vs `59572.5159772`).

## Root cause

Rakudo computes

```raku
method modified-julian-date { self.daycount + (($hour * 60 + $minute) * 60 + $second) / 86400 }
method julian-date          { self.modified-julian-date + 2_400_000.5 }
```

`$second` is a `Rat` and `2_400_000.5` is a `Rat` literal, so the whole
expression stays rational. mutsu's `julian_date`/`modified_julian_date` did the
same arithmetic in `f64` and wrapped the result in `Value::num`, which both
introduced binary-float noise and reported the wrong type.

The exact day fraction was already available: `day_fraction_rational` computes
it as a `(numerator, denominator)` pair for `.day-fraction`, leap-second days
included (divisor 86401 rather than 86400).

## Fix

Both methods now reuse `day_fraction_rational` and add through the ordinary
`Rat` arithmetic (`daycount + day-fraction`, then `+ 4800001/2`), so they
promote to big rationals instead of overflowing and return a `Rat`. The two
methods now share the day fraction with `.day-fraction` rather than
recomputing it in floating point.

Pinned by `t/numeric-coercion-gaps.t`: both `.WHAT`s, both exact values, and
that the two differ by exactly `2400000.5`.
