# A routine's declarator is a typed field, not `__method_literal` / `__mutsu_method_decl` markers

`my method foo` / `my submethod foo` lower to a routine plan, and a `method (...) { }` literal
compiles through the anonymous-routine path; both told the VM which declarator was written by
pushing marker traits (`__mutsu_method_decl`, `__method_literal`, `__submethod_literal`) that
`RoutineDeclarator::from_markers` and a closure-time `match` on trait names decoded again.

The named form carries the declarator in `DeclPass::declarator`; the literal passes it as the last
operand of `OpCode::MakeAnonSubParams`. Deleted: the three marker constants/traits,
`RoutineDeclarator::literal_marker`, `RoutineDeclarator::from_markers`, and the trait-name scans in
`register_sub` and the closure-building op.
