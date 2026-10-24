# Reverse and set-operator compound assignments are the typed node

Two more compound-assignment spellings move onto `ParsedExpr::CompoundAssign`
(#14948): the reversed infix and the set operators.

- **Reversed infix** (`$x R-= $y`, `$x R= $y`) assigns to its right operand.
  It lowers through `build_compound_assign_target_expr`. That is the
  expansion `EVAL` of a hand-built tree already used, and it now replaces the
  statement parser's own `MetaOp` writeback.
- **Set operators** (`$s (|)= 1`, `$s ∪= 1`) lower through the set
  operator's expansion.

Both go through `compound_lower`, the family's one lowering.

`rakuast::lower` reads `MetaInfix::Reverse` and a set infix into the same
node, so its own reverse reader is gone, along with
`parser::expand_reverse_assign_expr`.

A set-operator compound assignment written in expression position
(`say($s (|)= 2)`) still fails to parse. That predates this change and is
filed as #14967.

`t/rakuast/rakuast-reverse-set-compound-node.t` pins the shapes and the
semantics.
