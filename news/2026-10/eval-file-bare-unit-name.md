# `$?FILE` inside `EVAL` is the bare `EVAL_N`

A synthesized `EVAL` unit name is no longer absolutified against `$*CWD` for
`$?FILE`: `EVAL q[$?FILE]` now gives `EVAL_0`, `EVAL_1`, ... as rakudo does.
An explicit relative `:filename` is still joined onto `$*CWD`, as in rakudo.
