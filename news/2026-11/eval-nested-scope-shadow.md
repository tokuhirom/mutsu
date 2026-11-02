# An EVAL'd unit lets a nested scope shadow an outer routine

A `my sub` declared in a routine body or a nested block of an `EVAL`'d unit (and so of `EVAL(slurp($f).AST)`) replaced or
clashed with a routine of the same name declared at the unit's top level, which the unit's source text outside `EVAL` shadows
correctly (#15909). The unit is now judged per scope: a routine declared twice among the siblings of ONE statement list, at any
depth, is `X::Redeclaration` (`runtime/eval_scope_redecl.rs`), and the run-time registration lets a nested scope shadow.

`ast_visit::Visit` gained `visit_stmts`, called for every statement list the default recursion reaches, so an analysis that judges
statements against their siblings overrides one hook instead of re-implementing the walk. The `EVAL($ast)` ratchet pins the
69 more files this lets through.
