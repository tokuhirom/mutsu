# Negating a BigInt that lands back in i64 range no longer silently reads as 0 (plus a Rat-precision regression it surfaced)

```raku
my $big = 9223372036854775808;   # 2**63, one past i64::MAX -- parses as BigInt
say -$big;                        # -9223372036854775808 (i64::MIN, fits in Int)

enum CBORMinMax (CBOR_Min_NInt_63Bit => -9223372036854775808);
say +CBOR_Min_NInt_63Bit;         # raku: -9223372036854775808   mutsu (before): 0
say CBOR_Min_NInt_63Bit <= -1;    # raku: True                   mutsu (before): False
```

Found while investigating
`todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md`: the
vendored `CBOR::Simple` dist's own integer boundary constants
(`enum CBORMinMax (... CBOR_Min_NInt_63Bit => -9223372036854775808, ...)`)
made every negative-integer `cbor-encode` call pick the wrong branch of a
chained comparison, encoding plain negative integers (even `-1`) as
tag-3 BigInt instead of their compact native form.

## Root cause

`9223372036854775808` (2^63) is one past `i64::MAX`, so it parses as a
`BigInt`. `arith_negate` (`src/builtins/arith/pow_negate.rs`)'s `BigInt` arm
negated it and wrapped the result with the raw `Value::bigint` constructor —
`Value::bigint(-(**i).clone())` — instead of the normalizing
`Value::from_bigint`, which downcasts back to a plain `Int` whenever the
result fits in `i64` (exactly the case here: `-2^63` is `i64::MIN`, which
does fit). The negated value stayed BigInt-tagged even though it was, for
every practical purpose, an ordinary `Int`.

An enum declaration (`registration_sub.rs`) checks `ValueView::Int` to decide
whether a variant's value stores as `EnumValue::Int` or falls back to
`EnumValue::Generic` (a boxed catch-all `Value`) — a still-BigInt-tagged
`-9223372036854775808` took the `Generic` path. `EnumValue::as_i64()`
(`src/value/value_enum.rs`), the numeric-extraction helper every fast-path
`ValueView::Enum` numeric coercion uses (unary `+`, binary `+`, comparison
operators, `.polymod`, `.abs`, ...), simply returned `0` for any `Generic`
variant — it never tried to numify the wrapped value at all. `.Int`/`.Numeric`
method calls happened to be unaffected (they go through `EnumValue::to_value`,
which correctly unwraps `Generic`), which is why the bug looked narrower than
it was until the fast paths were checked directly.

## Fix

- `arith_negate`'s `BigInt` arm now uses `Value::from_bigint`, so a BigInt
  negation that lands back in `i64` range downcasts to a plain `Int`
  immediately, matching every other BigInt-producing arithmetic op's existing
  normalization convention.
- `EnumValue::as_i64()` now numifies a `Generic` variant's wrapped value
  (`Int`/`BigInt`/`Num`/`Bool`) instead of unconditionally returning `0`, as
  defense in depth — any other path that produces a `Generic`-stored enum
  value (not just this specific BigInt-negation shape) now numifies
  correctly too.

Found (but not fixed, out of scope) along the way: a `Rat`/`Num`-valued enum's
numeric coercion still truncates to an `Int` via a separate, unrelated bug in
`coerce_to_numeric`'s own `Enum` arm — filed as
`todo/tickets/rat-valued-enum-numeric-coercion-loses-fraction.md`.

## A second bug the fix surfaced: `Int + Rat` precision loss on numerator overflow

Making `-2**63` correctly a plain `Int` (instead of accidentally staying
BigInt-tagged) exposed a **pre-existing, unrelated regression** in a
previously-whitelisted test (`t/bigrat-sort-compare.t`): `(-2**63 + 0.1).FatRat`
lost its `.9` fractional part, printing `-9223372036854775807` instead of
`-9223372036854775807.9`. Reproduces with any plain large-magnitude `Int`,
not just `-2**63`:

```
$ mutsu -e 'say (9223372036854775807 + 0.1).FatRat'   # i64::MAX + 0.1
9223372036854775807          # before this fix — the .1 vanished
$ raku  -e 'say (9223372036854775807 + 0.1).FatRat'
9223372036854775807.1
```

Root cause: `rat_from_i128_or_num` (`src/builtins/arith/rat.rs`), the
overflow-checked i128 path `rat_add_checked`/`rat_sub_checked`/`rat_mul_checked`/
`rat_div_checked` all funnel through, degraded to a lossy `Num` whenever
EITHER the numerator or the denominator failed to fit back into `i64` after
GCD reduction. Real Rakudo's `Rat` only has that size constraint on its
**denominator** — the numerator can be arbitrarily large while `.WHAT` still
reports `Rat` (Rakudo backs it with a bigint numerator internally). mutsu
already has the correct distinction implemented in `make_big_rat_arith`
(`src/value/mod.rs`) — used by the separate BigRat/BigInt arithmetic path —
which only degrades to `Num` when the **denominator** specifically exceeds
`u64` range, promoting to `BigRat` otherwise. `rat_from_i128_or_num`
duplicated a NARROWER (and wrong) version of the same check instead of
reusing it; now delegates to `make_big_rat_arith` directly, removing the
duplicate GCD/overflow logic entirely.

Regression tests: `t/bigint-negate-and-rat-overflow-precision.t`.
