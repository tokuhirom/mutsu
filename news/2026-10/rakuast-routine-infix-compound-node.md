# A word or user-infix compound assignment is the typed compound node

`$n mod= 5` and `$x +%+= 5` (over a user-declared `infix:<+%+>`) move onto
`ParsedExpr::CompoundAssign` (#14948). The infix has no built-in compound
operator, so the node's `op` names the routine the assignment calls.

`compound_lower` lowers such a node:
- in expression position, to `build_custom_compound_assign_expr`'s expansion;
- as a statement on a variable, to the `Stmt::Assign` the statement parser
  built.

Four parser sites build the node through `written_routine_compound_assignment`:
- the statement path in `assign_stmt`;
- the expression path in `try_assign`;
- parenthesized targets in `paren`;
- the operand of a looser operator in `logic`.

`rakuast::lower` reads `MetaInfix::Assign(Infix("mod"))` into the same node.
Before this, the reader rejected such a tree as an unknown compound operator.
Its fallback in `lower_compound_assign_expr` could only reject, so it is
deleted.

`.AST` used to render the statement form as a plain `Assignment` of the
expansion (`$n = $n mod 5`). It now gives `MetaInfix::Assign(Infix("mod"))`,
as rakudo 2026.09 does.

`t/rakuast/rakuast-routine-infix-compound-node.t` pins the shapes.
