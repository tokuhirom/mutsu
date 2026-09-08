# A `Rat` with an over-64-bit denominator renders rakudo's digits

```raku
my $a = .1234567890123456789012345;
say $a.Str;
# raku:  0.1234567890123456824475648
# mutsu: 0.12345678901234568
```

The *value* was already right — `.numerator`, `.denominator` and `.raku` were
byte-identical to rakudo, so the literal parsed exactly. Only the rendering
diverged, and only for a denominator that does not fit 64 bits: mutsu shortcut
such a `Rat` through its `Num` value and printed f64's 17-digit shortest
round-trip.

## What rakudo actually does

`Rat` is `Rational[Int, uint64]`, so an arithmetic result whose denominator does
not fit `uint64` degrades to `Num`. `Rational.Str` reaches its fractional part
through such an operation, which means the digit budget it then applies
(`|denom| < 100_000 ?? 6 !! chars(|denom|) + 1`) is filled from **f64**, not from
the exact fraction. So rakudo's 25 digits are neither the exact expansion
(`0.1234567890123456789012345`) nor f64's shortest form — they are
`round(fract * 10^digits)` evaluated in f64 and then zero-stripped.

`FatRat` is `Rational[Int, Int]` and never degrades, so it keeps the exact
expansion. That contrast is what pins the rule: the same numerator/denominator
pair renders `0.1234567890123456824475648` as a `Rat` and
`0.1234567890123456789012345` as a `FatRat`, and the boundary is exactly
`uint64` — `2^64-1` is still exact, `2^64` is not.

## The fix

`format_rat_str_bigint` (`src/value/display.rs`) grew that degradation as
`f64_scaled_fraction`: for a non-FatRat whose denominator exceeds `u64::MAX`, the
scaled rounding runs in f64 the way rakudo's degraded arithmetic does. The
`BigRat` arm of `to_string_value` no longer shortcuts a big-denominator `Rat`
through `Num` at all; it hands every `BigRat` to the digit-budget formatter,
which now owns the whole rule. A fraction that underflows f64 still renders as
its whole part (`Rat.new(1, 10**400).Str` is `0`, as in rakudo), and a scale too
large for f64 falls back to the exact expansion — rakudo is not self-consistent
that far out, so staying finite beats emitting `Inf`.

The sign row of the ticket's matrix turned out to be a second, adjacent bug:
`prefix:<->` on a big-denominator `Rat` literal produced a **FatRat**, because
`arith_negate` routed it through the *arithmetic* constructor and
`is_fat_rat_like` reads an over-`uint64` denominator as proof of FatRat-ness.
Negation cannot change the denominator, so it never degrades; the `BigRat` arm
now consults the stored FatRat flag (which its own comment already calls
authoritative) and uses the non-degrading constructor.
`(-1.1234567890123456789012345).^name` is `Rat` again.

Pinned by `t/big-denominator-rat-str.t`, whose 27 assertions pass unchanged
under rakudo. `t/decimal-literal-big-integer-part.t`, which pins the value half,
is unmoved.

Closes #7577.
