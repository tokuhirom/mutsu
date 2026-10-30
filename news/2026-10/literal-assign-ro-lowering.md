# Assigning to an immutable literal is the assignment node plus one lowering (ADR-15334)

`120 = 3` was desugared by the parser into a `{ RHS; __mutsu_assignment_ro(LITERAL) }` block, and `convert`
took the block apart by shape (`literal_assign_ro_parts`) to render the written assignment.

The parser now builds the ordinary `ParsedExpr::Assign` for it, and `assign_lower` expands a literal target.
The two expansions (`assignment_ro_expr`, `literal_assign_ro_expr`) and the reserved call name live in
`rakuast::assign_ro`.

Deleted: the three `"__mutsu_assignment_ro"` literals in `src/parser` (`internal-name-literals` 39 -> 38), the
shape recognizer and its `convert` arm (`convert-lines` 6875 -> 6871). Parity test
`t/rakuast/rakuast-stage2-literal-assign-paths.t`.
