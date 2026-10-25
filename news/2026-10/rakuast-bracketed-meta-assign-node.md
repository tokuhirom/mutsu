# A bracketed metaoperator assignment is the typed compound node

`@a [X+]= 1`, `@a [Z+]= …` and `$s [R~]= …` move onto `ParsedExpr::CompoundAssign`
(#14948). `CompoundMeta::BracketedMeta(kind, depth)` keeps the metaoperator
(cross, zip, reverse, or `Reduce` for a bare non-compound infix) and the
bracket depth.

`compound_lower` lowers it:
- in expression position, to the `AssignExpr` of the metaoperator's value;
- as a statement, to the `Stmt::Assign` the statement parser built.

`rakuast::lower` reads `MetaInfix::Assign(BracketedInfix(MetaInfix::*))`
into the same node.

The last users of two Stage 1 devices are gone, so both are deleted:
- the `Spelling::BracketedAssignment` record, which kept the bracket depth
  beside the executable `MetaOp`;
- the hidden `source-stmt-assign-name` field, which `convert` attached so
  `lower` could rebuild a statement assignment (`compound_stmt::mark`,
  `restore_stmt`, `is_source_field`).

The shapes and results are unchanged. They match rakudo 2026.09.

`t/rakuast/rakuast-bracketed-meta-assign-node.t` pins them.
