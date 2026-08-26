# Generic `Real` arithmetic now bridges BOTH operands, so a `Real` subclass mixed with a built-in number yields a `Num`

Adding the doc's `Temperature` example (`raku-doc/doc/Type/Real.rakudoc`, a
`class Temperature is Real` with only a `method Bridge`) produced an exact `Rat`
in mutsu (`593431/90`, printed as the digit-budget-truncated `6593.677778`)
where rakudo produces the approximate `Num` `6593.677777777778`.

## Root cause

Rakudo's fallback candidate for two `Real`s is written in terms of `.Bridge`:

```raku
multi sub infix:<+>(Real \a, Real \b) { a.Bridge + b.Bridge }
```

and every *built-in* numeric type's `.Bridge` is `self.Num` — `3.Bridge` is
`3e0`, `(1/2).Bridge` is `0.5e0`; only `Num.Bridge` returns `self`. So the rule
is a property of the **pair**, not of either operand alone:

* `T + T` (both `Bridge`s returning `Rat`) stays an exact `Rat` — `0.75`.
* `T + 1/4` is a `Num` — the `Rat` operand goes through `.Bridge` too, and
  `Rat + Num` is a `Num`.
* `T + 2` where `T.Bridge` returns an `Int` is `3e0`, not `3`.
* Two `Bridge`s that both return `Int` still add to an `Int`.

A plain (non-`Real`) object with a user `method Numeric` is deliberately *not*
part of this rule: rakudo numifies it through `.Numeric` and leaves the other
operand exact, so `F.new + 1/4` is an exact `Rat` there.

mutsu's `coerce_numeric_bridge_value` decided this per operand: it bridged the
`Instance` and handed the built-in number back untouched, so `Rat + Temperature`
stayed exact. That is what made the doc example's *running* sum exact — the
first `Temperature + Temperature` correctly produced a `Rat`, and the third
addition (`Rat + Temperature`) then failed to demote to `Num`.

A second, smaller divergence sat next to it: mutsu tried `Numeric` before
`Bridge` for every object. For a `Real` subclass that defines both, rakudo uses
`Bridge` — the generic candidates are spelled that way.

## Fix

`coerce_numeric_bridge_pair` now decides the bridge for the pair. When either
operand is an object that does `Real`, the *other* operand is additionally put
through the built-in `Real.Bridge` (`.Num` for `Int`/`Rat`/`FatRat`/`Bool`,
identity for `Num`); when neither is, both operands are left exactly as the
per-operand bridge produced them. `coerce_infix_operand_numeric` also prefers
`Bridge` over `Numeric` for a `Real`-doing object.

Pinned by `t/numeric-coercion-gaps.t`, which asserts `.WHAT` as well as the
value for `+ - *` across `T+T`, `T+Rat`, `Rat+T`, `T+Int`, `T+Num`, the
`Bridge`-beats-`Numeric` case, the non-`Real`-with-`Numeric` case, and the
doc's chained `Temperature` sum.
