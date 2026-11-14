# A plain `sub infix:<op>` import of a built-in operator is block-lexical

A `use` inside a block that imports a plain (non-`multi`) `sub infix:<+>`,
`infix:<**>`, `infix:<==>` and friends now replaces the operator in that block
only, like the `multi` form already did (ADR-8751 stage 2). A routine written
outside the block no longer picks the import up when it is called from inside.
The imported family shadows the built-in, so it is not ranked against the core
candidates.

`infix:</>` was the odd one out: its name contains the `/` that separates a
multi candidate's registry key, so `toplevel_global_routine_name` took the plain
key for a candidate and left the alias in `GLOBAL::`. The `/` inside an
operator's `<...>` is no longer a separator.

With this, the Wheel distribution's `t/03-lexical.rakutest` and
`t/07-power.rakutest` pass (#17767).
