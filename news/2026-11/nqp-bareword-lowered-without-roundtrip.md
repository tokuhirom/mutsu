# An `nqp::` bareword is lowered by the classifier's answer

`nqp::time` and `nqp::const::NAME` written as barewords were the largest class
of words the executing finalization converted to RakuAST and lowered back (228 of
the 795 bareword conversions over 700 `t/` files). `bareword::view_node` is the
one classifier; for the `Nqp` and `Nqp::Const` answers the lowering is a pure
function of the word (the argument-less `nqp::op` call, or the word itself), so
`bareword::nqp_term` returns that expression and `Finalize` uses it without
building and reading back the node tree (ADR-15760 slice 2). A word a unit
declares or imports under the same name is still resolved by the classifier first.
`.AST` and `EVAL($ast)` are unchanged.
