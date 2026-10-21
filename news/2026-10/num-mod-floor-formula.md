# `Num % Real` computes `$a - floor($a / $b) * $b`

mutsu's floored `Num` modulus used `fmod` with a sign fix-up, which is exact
where Rakudo's formula `$a - floor($a / $b) * $b` is not: `1e0 % 0.3` is
`0.10000000000000009`, `1e0 % 0.1e0` is `0`, and an infinite divisor gives
`NaN` (`3e0 % Inf`) instead of the dividend. `float_mod_floor` now uses the
formula, so every `Num` modulus (and the mixed `Int`/`Rat`/`Num` forms that
convert to `Num`) answers as Rakudo does (#14195).
