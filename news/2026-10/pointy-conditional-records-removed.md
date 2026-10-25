# Pointy conditionals no longer keep a source record

`if`/`with`/`given` with a pointy parameter used to open their expansion with a
compiler-inert `SourceForm::IfPointy` / `WithPointy` / `GivenPointy` record that
duplicated the written parameters and body, and `eval_var_scan`, `bareword`
and the visitors read it back. The typed `ParsedStmt::Conditional` / `Given`
nodes already carry the written shape for the RakuAST view, so the three
variants, their writers, readers and `convert` arms are deleted. The copy also
doubled the tree at every nesting level, which needed a size budget; that budget
is gone for these constructs. `SourceForm` is down to `RoutineTraits` and
`DeclarationCompoundAssign`.
