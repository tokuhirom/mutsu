# The last plain-assignment arms of the RakuAST converter are gone

An assignment in a no-paren call argument (`one $x = 2 > 1 ?? "P" !! "Q"`) was
the last `=` the parser built as an untyped `AssignExpr` in a `.AST` parse. It
now goes through `written_assignment`, like every other plain assignment
(#14948). The `.AST` text and the results are unchanged; they match rakudo
2026.09.

A probe run over the RakuAST ratchet list and `t/rakuast` found that nothing
reached the plain-assignment arms of `rakuast::convert` any more, so they are
deleted:
- the `Stmt::Assign` arm for `=` (only the `:=` arm stays);
- the plain-variable tail of the `Expr::AssignExpr` arm (a bound, term or `*`
  lvalue still renders there);
- both `Expr::MultiDimIndexAssign` arms (`@a[0;1] = 5`, `@a[|| @i] = 5`);
- `assignment_infix`, which only they called.

`t/rakuast/rakuast-listop-arg-assign-node.t` pins the argument form and the
multi-dimensional forms.
