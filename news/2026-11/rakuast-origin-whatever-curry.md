# RakuAST: origins of whatever-curried infixes and calls on a term

`* + 1`, `3 < *` and `(* + 1)(2)` now carry the ranges rakudo gives them. The
priming `*` has no origin of its own, but the `ApplyInfix`, its `Infix` and its
`ArgList` span it; the `*` is located in the source next to the other operand
and the operator. A `Call::Term` runs from its `(` to its `)`, with the
`ArgList` inside. On `rakuast-whatever-curry-finalize.t` the nodes rakudo has
and mutsu lacked went from 27 to 5 (`(* + *)(3, 4)`, whose two `*` give no
anchor, remains; see #15780).
