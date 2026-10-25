# A compound assignment is a typed parser node with one lowering

The compound-assignment family moves onto the Stage 2 mechanism (#14948).

`ParsedExpr::CompoundAssign` keeps `TARGET OP= SOURCE` as written, along with
how the infix was spelled: plainly, inside brackets (`[+]=`, `[[+]]=`), or as
a scalar's `Z+=`.
- A `.AST` parse keeps the node.
- Every other parse lowers it where it is built.

`stmt::assign::compound_lower` is now the family's single lowering:
- **Expression form:** the source marker over the expansion that
  `build_compound_assign_expr` builds for the target. `//=`, `||=` and `&&=`
  on a scalar short-circuit there.
- **Statement form:** a `Stmt::Assign` for a variable.

Before, the statement parser kept its own expansions for a subscript, a
method-call and a ternary target. The expression parsers and `EVAL` each
took a slightly different route. All of these now go through this lowering.
`rakuast::lower` reads a hand-built `MetaInfix::Assign` into the same node,
so a round-tripped `$_ *= 10` is a statement assignment again without the
hidden source field the conversion used to attach.

A scalar's `Z+=` now renders `MetaInfix::Assign(MetaInfix::Zip(Infix("+")))`,
as rakudo 2026.09 does. Before, the zip was dropped.

`t/rakuast/rakuast-compound-assign-node.t` pins the shapes and the
semantics.
