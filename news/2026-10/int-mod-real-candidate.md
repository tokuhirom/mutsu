# `Int mod Real` follows Rakudo's `Real, Real` candidate

With an `Int` left side and a `Rat`, `FatRat` or finite `Num` right side, `mod` answered the mathematical floored remainder (`7 mod 2.5` was `2`). Rakudo picks `$a - ($a div $b) * $b`, where `div` coerces `$b` to `Int` first, so it answers `-0.5` (and divides by zero when `$b.Int` is 0). `arith_mod` now computes exactly that, so the infix, `[mod]` and the metaops agree. `7 div 2.5.FatRat` also stopped dividing by zero: `Value::to_bigint` had no `FatRat` arm.
