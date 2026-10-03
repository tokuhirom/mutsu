# `so` / `not` take a whole conditional as their operand

The loose prefixes `so` and `not` parsed their operand at item-assignment
level without the conditional, so `so 0 ?? 1 !! 0` became `(so 0) ?? 1 !! 0`
and printed `0` instead of `False`. Loose unary is looser than `?? !!`; the
operand is now a whole item-level expression, so `so A ?? B !! C` is
`so(A ?? B !! C)` as in Rakudo, while the comma and `and`/`or` still end it
(#11478).
