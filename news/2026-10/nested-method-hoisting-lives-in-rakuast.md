# Nested-block method hoisting lives in `rakuast`

ADR-15334 (reading B), nested-methods family. `hoist` (a `method` declared in a nested block of a package
body becomes the package's, its in-block declaration a `Stmt::NestedMethodCapture`) and its inverse
`unhoist` (the body as written, for `.AST`) moved from `src/parser/stmt/nested_block_methods.rs` to
`src/rakuast/nested_methods.rs`. The parser's class / role / grammar / anonymous-declaration / statement-list
sites and the RakuAST reader and lowering call them there; the `parser::{hoist,unhoist}_nested_methods`
re-exports are gone.

`t/rakuast/rakuast-stage2-nested-methods-paths.t` pins file / `EVAL` string / `EVAL` of `.AST` for the
forms that work on all three paths, and runs the rest on the file path only: a nested-block method that
closes over a `my` variable dies inside `EVAL` ([#15468](https://github.com/tokuhirom/mutsu/issues/15468),
unchanged by the move). Ratchets: `synthetic-block-sites` 55 to 54, `convert-lines` 6923 to 6917.
