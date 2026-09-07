# A `Rat` with an over-64-bit denominator stringifies through f64

Found 2026-09-07 while fixing
`todo/tickets/decimal-literal-with-big-integer-part-loses-it.md`
(`news/2026-09/decimal-literal-keeps-its-big-integer-part.md`). That fix made a
long-fraction literal reachable at all — it used to panic — so this rendering
gap is newly *observable*, not newly wrong: it is the pre-existing `Rat.Str`
behaviour for a denominator that does not fit 64 bits.

## Repro

```raku
my $a = .1234567890123456789012345;
say $a.numerator ~ "/" ~ $a.denominator;
# both: 246913578024691357802469/2000000000000000000000000
say $a.raku;
# both: <246913578024691357802469/2000000000000000000000000>
say $a.Str;
# raku:  0.1234567890123456824475648
# mutsu: 0.12345678901234568
```

Same for a nonzero integer part: `1.1234567890123456789012345.Str` is
`1.1234567890123456824475648` in raku and `1.1234567890123457` in mutsu.

## Narrowed

The *value* is right — `.numerator`, `.denominator` and `.raku` are
byte-identical to raku, so the literal parses exactly. Only `Rat.Str` diverges,
and only when the denominator exceeds 64 bits: mutsu renders f64's shortest
round-trip (17 significant digits), raku renders 25 decimal places.

Note raku's 25 digits are NOT the exact decimal expansion of that fraction
(which is `0.1234567890123456789012345`) — they are the f64 value re-expanded
to the fraction's own decimal length. So this is not "mutsu rounds, raku is
exact"; both go through f64, and the question is how many digits are emitted
afterwards. Work out raku's rule from `Rat.Str` before implementing: it is
plausibly "expand the f64 to `denominator`'s number of decimal places", which
is what the two data points above are consistent with.

Computing the same value with `/` instead of a literal produces a `Num` in both
(`246913578024691357802469 / 2000000000000000000000000` is `(Num)`), and there
the two agree — so this is reachable only through the literal path (and through
`Rat.new` with big parts).

## Where to look

The `Rat`/`FatRat` `Str`/`gist` rendering — `src/value/` and the numeric
stringification helpers — for the branch taken when the denominator does not
fit `i64`/`u64`.

## Check when fixing

The two repros; the sign case (`-1.1234567890123456789012345`); a denominator
that fits 64 bits (`0.1234567890123456789` — 19 digits, already correct in both,
must not change); `.gist` alongside `.Str`; `.raku` and
`.numerator`/`.denominator`, which are already exact and must stay so; and
`t/decimal-literal-big-integer-part.t`, which pins the value half.
