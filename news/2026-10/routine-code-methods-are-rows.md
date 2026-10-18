# `Routine` and `Code` methods answer through rows

`wrap`, `unwrap`, `assuming`, `candidates`, `dispatchees`, `add_dispatchee`,
`cando`, `rw`, `prec`, `precedence`, `associative` and
`is-implementation-detail` of a code object are method-table rows of `Routine`
and `Code` (ADR-11276 §9.166). Each used to be written twice by name, once for a
`&name` handle and once for a `Sub`; a row now picks the body for the kind, and
`.^can` and dispatch agree on the owner Rakudo declares them on.
