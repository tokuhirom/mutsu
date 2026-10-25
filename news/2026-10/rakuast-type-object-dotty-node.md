# `C .= new` on a type object is the typed `.=` node

The statement parser used to build `C .= new` on a type object as a hand-made
`Stmt::Assign` to `C`. That is what makes it die with "Cannot modify an
immutable 'C' type object". `.AST` rendered it as a plain assignment of
`C.new` to a `$C` variable.

The statement now builds `ParsedExpr::CompoundAssign` with `CompoundMeta::Dotty`
over the bareword (#14948). The shared lowering's new statement form,
`compound_lower::lower_dotty_statement`, turns a bareword that names no
sigilless term into that same assignment. So the program still dies as on
rakudo, and `.AST` gives `ApplyDottyInfix(left => Type::Simple, infix => DottyInfix::CallAssign)`,
as rakudo 2026.09 does.

`rakuast::lower` lowers a statement `ApplyDottyInfix` through the same
statement form. It reads the node in one place (`dotty_parts`), whether the
result is an expression or a statement.

`t/rakuast/rakuast-type-object-dotty-node.t` pins the shape and the
exception.
