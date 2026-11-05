# `Parameter.^attributes` exposes the `$!flags` bit field

A `Parameter` now carries rakudo's `$!flags` word (an `Int` of `SIG_ELEM_*`
bits) and lists it through `.^attributes`, so MOP code can read and write it
with `get_value` / `set_value`. The word is computed from the declaration and
matches rakudo for positional, named, slurpy (`*@`, `**@`, `+@`, `*%`),
capture, coercion, definedness-smiley, `is rw`/`copy`/`raw` and invocant
parameters. `nqp::const::SIG_ELEM_*` are compile-time constants, and
`.optional` reads the `SIG_ELEM_IS_OPTIONAL` bit, so a
`trait_mod:<is>(Parameter, :$injected)` that sets it (Deps) takes effect.
Deps 0.0.1 `t/01-basic.rakutest` goes from 30 to 72 of rakudo's 74 passing.
