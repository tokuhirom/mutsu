# Int-based enum values now satisfy a `UInt` type check

`INTERNAL_ERROR ~~ UInt` (where `INTERNAL_ERROR` is a value of an
Int-based `enum`) returned `False` and constructing an object with a
`UInt`-typed attribute from an enum value died with a type check error,
even though the underlying enum value was a non-negative Int and raku
accepts it.

## Root cause

`Interpreter::type_matches` (`src/runtime/types/type_matching.rs`) has an
early-return fast path for the `UInt` constraint that matched `Int`,
`BigInt`, `Nil`, and `Package` views directly, falling through to `false`
for everything else — including `ValueView::Enum`. The generic
enum-compatibility check later in the same function never ran because the
`UInt` arm already returned. In Raku, an Int-based enum value literally
*is* an Int (`UInt` is `subset UInt of Int where * >= 0`), so any
non-negative enum value must match.

## Fix

Added an `Enum { value: EnumValue::Int(i), .. }` arm to the `UInt` match
that accepts non-negative values, mirroring the existing `Int`/`BigInt`
arms.

## Verification

- `INTERNAL_ERROR ~~ UInt` now returns `True`, matching raku.
- `my UInt $u = INTERNAL_ERROR` and `RstStream.new(error-code =>
  INTERNAL_ERROR)` (a `has UInt $.error-code` attribute) no longer die.
- New pin: `t/enum-uint-subset.t`.
- Whitelisted `S12-enums/*.t` and `S02-types`/`S12-subset` roast files
  pass with no regressions.
