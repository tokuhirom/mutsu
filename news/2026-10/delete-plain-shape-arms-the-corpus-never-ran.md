# Plain-shape arms the whole corpus never ran are deleted

With every parse entry returning the round-tripped tree, the parser's own output holds conditionals,
assignments and loops only in their kept (typed) shape. A coverage run of the whole `t/` suite plus the roast
whitelist (instrumented debug build, `docs/dead-code-coverage.md`) showed the arms that still matched the
old plain shapes never ran, and they are deleted: the comma-list continuation of a parenthesized
assignment (`assign/paren.rs`, 88 lines), `regroup_assign_expr_metaop_rhs` and `split_assignment_rhs_call_args`
with the call-argument expansion built on them (`stmt/args.rs`), the plain `Stmt::If` constant-folding arm and
`has_elsif_chain` (`stmt/trace.rs`), the three never-taken arms and the helper of
`compose_prefix_into_whatevercode` (it is now a plain `Expr::Unary`), and the plain `AssignExpr` arms of the
list-assignment helpers (`assign/sink.rs`).
