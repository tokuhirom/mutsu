# A Rat/Num-valued enum's numeric coercion (unary `+`, binary `+`, etc.) truncates to an Int

Found while writing a regression test for
`news/2026-08/bigint-negate-i64-min-downcast.md` (unrelated fix).

## Repro

```raku
enum RatE (Half => 1/2);
say +Half;
```

```
raku:  0.5
mutsu: 0
```

## Root cause

`coerce_to_numeric` (`src/runtime/utils/radix_numeric.rs`)'s `ValueView::Enum`
arm unconditionally does `Value::int(value.as_i64())` — forcing the result to
a plain `Int` regardless of what the enum's underlying value actually is.
For an `Int`-valued enum this is correct; for a `Rat`/`Num`-valued enum (any
`EnumValue::Generic` wrapping a non-Int numeric type) it silently truncates
away the fractional part.

## Suggested fix

`coerce_to_numeric`'s `Enum` arm should numify via `value.to_value()`
followed by a recursive `coerce_to_numeric` call (or an equivalent that
preserves the wrapped type), not `Value::int(value.as_i64())` specifically.

## Severity

Low: a Rat/Num-valued enum is an unusual shape (most enums are Int- or
Str-valued); no roast test currently depends on this.
