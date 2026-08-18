# A Rat/Num/Str-valued enum's numeric coercion no longer truncates to an Int

```raku
enum RatE (Half => 1/2);
say +Half;
```

```
raku:  0.5
mutsu (before): 0
```

## Root cause

`coerce_to_numeric` (`src/runtime/utils/radix_numeric.rs`)'s `ValueView::Enum`
arm unconditionally did `Value::int(value.as_i64())` — forcing the result to
a plain `Int` regardless of what the enum's underlying value actually is. For
an `Int`-valued enum this happened to be correct; for a `Rat`/`Num`-valued
enum (any `EnumValue::Generic` wrapping a non-Int numeric type) it silently
truncated away the fractional part. As a side effect, a `Str`-valued enum
also always numified to `0` instead of parsing the string numerically the
way real `raku` does (`enum E (A => "5"); +E::A` is `5` in raku, was `0` in
mutsu).

## Fix

The `Enum` arm now numifies the enum's own underlying value recursively
(`coerce_to_numeric(value.to_value())`) instead of forcing through
`as_i64()`: an `Int`-valued enum stays exact, a `Str`-valued enum now parses
numerically (reusing the same `Str` arm every other string numeric coercion
already goes through), and a `Rat`/`Num`/`BigInt`-valued enum keeps its
real value instead of truncating.

Regression test: `t/enum-rat-num-value-numeric-coercion.t` (6 assertions,
all verified against real `raku`), covering unary/binary `+` on a
Rat-valued enum, a Num-valued enum, an Int-valued enum (regression guard), a
numeric-string-valued enum, and the BigInt-boundary-valued enum shape from
the `bigint-negate-i64-min-downcast` fix (regression guard).
