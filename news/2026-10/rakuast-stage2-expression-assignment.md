# An assignment in expression position is a typed parser node

ADR-10723 Stage 2 reaches expressions (#14948). The parser now has the
expression carrier `Expr::Ast`, and its first node is the assignment:
`ParsedExpr::Assign` keeps the target as written, parentheses included, and
the source.

The `=` of an assignment in expression position builds this node. An
executing parse lowers it at once through `rakuast::assign_lower`, which
now holds the per-target dispatch the parser used to do inline:

- a variable becomes an `AssignExpr`;
- a subscript becomes an `IndexAssign`;
- a call or a parenthesised list becomes a container writeback;
- a method call becomes the rw-accessor writeback.

A `.AST` parse keeps the node and renders it as rakudo 2026.09 does: an
`ApplyInfix` over `Assignment`. An assignment to a positional, literal hash
or single-dimension subscript folds into the subscript's `assignee` instead.

A hand-built `ApplyInfix(Assignment)` over such a target lowers through the
same function. This also lets `EVAL` of `nqp::atposref_i(@i, 0) = 5` work.
That tree now renders without the parentheses mutsu used to invent around
the op.

Parser passes that still read the executable shape see the node lowered:
`temp`, the ternary-branch check and the fat-arrow repair. Each of them moves
to the node in a later slice.

`t/rakuast/rakuast-expression-assignment-node.t` pins the shapes and the
semantics.
