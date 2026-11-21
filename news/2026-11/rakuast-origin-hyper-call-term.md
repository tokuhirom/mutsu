# `.origin` of a hyper call on a term

The `Call::Term` that a hyper postfix holds (`@a».(3)`, `@a>>.(1, 2)`,
`@a».()`) now carries the ranges rakudo 2026.11 gives it. The call runs from
its `(` to its `)`. Its `ArgList` covers the arguments, or is empty just
inside the `(` when there are none. The measuring and placing in
`call_term_origins` are shared with the plain `$f(...)` / `$f.(...)` call on a
term (#15780).
