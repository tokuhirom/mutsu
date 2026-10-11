# The numeric conversions, base and polymod are method-table rows

`Numeric`, `Real`, `Rat`, `FatRat` and `Complex` of `Int`, `Num`, `Rat` and `Complex`, `base`/`base-repeating` of the numbers, `polymod` of the `Real`
types and `Complex`'s conversions to a `Real` type are rows now (ADR-11276 §9.70, #12675), each sharing one routine with the cascade arm it overlaps.

Along the way mutsu moved toward Rakudo: `(2**70).Rat` is no longer clamped to a machine word, `Bool.Complex` is `1+0i`, `Bool.base` answers, `.base`
of a big Int takes a digits count, `NaN.base(16, 3)` fails like `NaN.base(16)` instead of answering `"NaN"`, a bad radix reports the value and the
range, and `Rat.base-repeating` works without an argument.
