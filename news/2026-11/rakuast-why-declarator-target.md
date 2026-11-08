# `WHY` answers `Nil` on nodes that are not declarator targets

`WHY` on a RakuAST node that is not a `RakuAST::Doc::DeclaratorTarget` (a literal, a statement, a
name, a statement list, ...) is `Nil`, as on rakudo 2026.09; the `Doc::Declarator` type object is
only for the classes that take docs. A parsed declaration's `Doc::Declarator` prints only the
`leading` / `trailing` part its source wrote (it used to print an empty `trailing => ()`).
