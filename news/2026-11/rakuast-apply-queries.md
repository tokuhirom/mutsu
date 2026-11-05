# RakuAST: operator applications answer `operator`, `operands` and `return-type`

`ApplyInfix`, `ApplyPrefix`, `ApplyPostfix` and `ApplyListInfix` now answer the three questions
rakudo 2026.09 does (`src/rakuast/apply_queries.rs`; rows in
`src/builtins/method_table/instances/rakuast_apply_queries.rs`): `operator` is the infix / prefix /
postfix node (a method call is a `Call::Method`), `operands` is the left and right of an infix, the
one operand of a prefix or postfix, or the arguments of a list infix, as a List, and `return-type`
is the `Mu` type object. Matches `raku` on 9 parsed shapes and a hand-built `ApplyInfix`. Left:
`return-type` on other expression classes (rakudo's `Ternary` has it too). Test:
`t/rakuast/rakuast-apply-queries.t`; the method-table pin in `rakuast-type-method-table.t` lists the
three new methods.
