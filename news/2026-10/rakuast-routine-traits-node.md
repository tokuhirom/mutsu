RakuAST Stage 2: routine traits become a typed parser node; `SourceForm` is deleted

The traits written on a routine or package (`is export`, `returns`, `handles`, ...) are kept as
`ParsedStmt::RoutineTraits`, a compiler-inert node that opens the body of a `.AST` parse, instead of a
`Stmt::SourceForm` record. An ordinary parse builds none. With its last variant gone, the `SourceForm` type,
`Stmt::SourceForm`, `Visit::visit_source_form`/`walk_source_form` (now `visit_routine_traits`/`walk_routine_traits`)
and the `source_form` recognizer with its two unreachable `convert.rs` arms are deleted.
