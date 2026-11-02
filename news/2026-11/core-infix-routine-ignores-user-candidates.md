# `CORE::<&infix:<eq>>` and its siblings ignore the program's candidates

A CORE routine value (`&CORE::infix:<op>`, `CORE::<&infix:<op>>`) runs the built-in
operator and never a `multi infix:<op>` the program declared (#15922); mutsu answered
`True` from the user's `eq` where rakudo answers `False`. The core routine form shared the
operators' bodies, which ask the user candidates first: `eq ne lt gt le ge`, `leg`, `min` and
`max`. Each now has a `_core` body without the probe (`str_cmp_values_core`, `str_leg_core`,
`min_max_values_core`), and `comparison_family_values` takes a `core_only` flag the routine form sets.
Pinned by `t/routines/dispatch/core-infix-routine-ignores-user-candidates.t`.
