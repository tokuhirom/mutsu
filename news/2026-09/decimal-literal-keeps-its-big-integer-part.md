# A decimal literal keeps an integer part past `i64::MAX`

```
$ mutsu -e 'say 1000000000000000000000000000000.5; say 12345678901234567890.5'
0.5
0.5                                    (before)

$ raku -e 'say 1000000000000000000000000000000.5; say 12345678901234567890.5'
1000000000000000000000000000000.5
12345678901234567890.5
```

The value was wrong, not merely imprecise, and nothing warned — the integer part
was simply dropped. The threshold was exactly `i64::MAX`: `9223372036854775807.5`
was right and `9223372036854775808.5` was not. A negative literal lost it the
same way (`-12345678901234567890.5` was `-0.5`).

## Root cause: the fallback was selected by the wrong signal

`src/parser/primary/number.rs` already had a correct BigInt arm building the
exact `make_big_rat(int * 10^frac_digits + frac, 10^frac_digits)`. It was only
reachable through an arithmetic-overflow check:

```rust
let int_val: i64 = int_clean.parse().unwrap_or(0);   // silently 0 past i64::MAX
let frac_val: i64 = frac_clean.parse().unwrap_or(0);
let numer = int_val.checked_mul(denom).and_then(|v| v.checked_add(frac_val));
```

An integer part above `i64::MAX` fails `parse::<i64>()`, and `unwrap_or(0)`
turned that into a `0` whose `0 * denom + frac` does **not** overflow — so the
`Some` arm produced `0.5` and the fallback never ran. The `unwrap_or(0)`
swallowed the one signal that should have selected it.

The arm is now selected by whether the parts *fit* an i64 rather than by whether
the arithmetic on them overflows. It is a routing fix; the BigInt arithmetic is
unchanged.

## The sibling branch panicked

The ticket asked to check the leading-`.` branch for the same shape. It had a
worse one: no digit-count guard at all, so `10i64.pow(frac_digits)` **panicked**
for a fraction longer than 18 digits —

```
$ mutsu -e 'say .1234567890123456789012345'
thread 'mutsu-main' panicked at core/src/num/mod.rs:414:5:
attempt to multiply with overflow
```

It now tests `checked_pow` and the parse together and takes the same BigInt
path, producing the exact `246913578024691357802469/2000000000000000000000000`
— byte-identical to raku's `.numerator`, `.denominator` and `.raku`.

## Split off, deliberately

Making that literal reachable exposed a *rendering* gap that is not this fix's:
`Rat.Str` for a denominator that does not fit 64 bits emits f64's shortest
round-trip (`0.12345678901234568`) where raku emits 25 decimal places
(`0.1234567890123456824475648`). The value is identical in both — only the
string differs, and computing the same fraction with `/` yields a `Num` in both,
where they agree. Filed as
`todo/tickets/big-denominator-rat-str-truncates-to-f64.md`.

## Measured against `raku`, all matching

Both repro literals; the `i64::MAX` / `i64::MAX + 1` boundary; the negative
form; a two-digit fraction; `.WHAT`, `.numerator` and `.denominator` of the big
literal; the small literals `1.5`, `0.5` and `.5`, unchanged; and the 25-digit
fraction that used to abort.

## Testing

New `t/decimal-literal-big-integer-part.t` (14 assertions), which passes
unchanged under rakudo.
