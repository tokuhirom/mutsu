# Mix and MixHash weights keep their own type

A `Mix`/`MixHash` weight used to be stored as an `f64` and decoded back by guessing,
so `(a => 1/3).Mix<a>` was a `Num` and printed `0.3333333333333333`. The weight is now a
`MixWeight` (`Int`, `Rat` or `Num`) stored as such: `(a => 1/3).Mix` renders `a(0.333333)`,
`.raku` gives `("a"=><1/3>).Mix`, and weights combine under the numeric tower
(`1/3 + 1/3` stays the `Rat` `2/3`, `$mh<a> += 1/3` is exact). The two duplicate `.total`
sums collapsed into the one in `builtins::mix_weight`.

Rats with a denominator beyond 64 bits still degrade to `Num`.
