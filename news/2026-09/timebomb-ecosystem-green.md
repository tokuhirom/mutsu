# TimeBomb 0.0.1 passes its ecosystem suite

The `TimeBomb` 0.0.1 distribution moved from a regression to parity: its
single baseline test now passes under mutsu as well as Rakudo.

The fixes cover `Date`/`DateTime` smartmatching against their string
representations and stale named wrap chains surviving an `EVAL` redeclaration.
They are pinned by `t/types/temporal/temporal-smartmatch.t` and
`t/routines/wrap-eval-redeclaration.t`.
