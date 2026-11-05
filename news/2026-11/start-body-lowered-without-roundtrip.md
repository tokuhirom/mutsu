# A `start` block with a rejected placeholder is lowered where the parser built it

The body of `start { $^a }` (a placeholder Raku rejects) was a typed
`ParsedExpr::StartBody` node that the executing finalization still converted to
RakuAST and lowered back. It is now carried and lowered in
`Finalize::start_body`: the body is lowered where it stands in a lexical scope of
its own, then wrapped in the do-block (`Expr::desugar_block`) that makes the
compiler report `X::Placeholder::Block`. `.AST` and `EVAL($ast)` are unchanged
(ADR-15760 slice 2).
