# `.Rat(eps)` / `.FatRat(eps)` bind `Real $epsilon`

`3.7e0.Rat('0.01')` answered `3.7`: the epsilon was read through a helper whose fallback was the default
`1e-6`, so a `Str`, `Nil`, a type object or a `Complex` was silently ignored
([#12043](https://github.com/tokuhirom/mutsu/issues/12043)). Rakudo's parameter is `Real $epsilon`, so
those fail the bind with `X::TypeCheck::Binding::Parameter` ("Type check failed in binding to parameter
'epsilon'; expected Real but got Str (\"0.01\")"). The parameter is named after the candidate that binds
it, as Rakudo reports it: `epsilon` for `Num.Rat` (and `Complex.Rat`, which forwards), `$epsilon` for
`Num.FatRat`, `<anon>` for the `Rat` candidates.

The same fallback also ignored every `Real` the helper did not read: a `Bool` (`3.7e0.Rat(True)` is
`3.0`), a `FatRat`, an `Int` too big for 64 bits, an allomorph (`<1>`, `<1.0>`) or a `Duration`. They now
numify, through an explicit `Real` check (`is_real_epsilon`) rather than the lenient `to_float_value`,
which also numifies a `Str`.

The epsilon is checked only by an invocant that binds it: an `Int` is already rational, so
`7.Rat('0.01')` still answers `7.0`, exactly as in Rakudo.

Test: `t/types/numeric/rat-fatrat-epsilon-real-check.t`. Closes
[#12043](https://github.com/tokuhirom/mutsu/issues/12043).
