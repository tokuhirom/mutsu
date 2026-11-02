# `src/rakuast/convert` is `view`

RakuAST C1 step 3 (#15759). The module that renders the parser's tree as RakuAST nodes (the read direction, what `Str.AST` runs) is
named for what it is after ADR-15334 Amendment 4/5: `src/rakuast/convert.rs` and `regex_convert.rs` are `view.rs` and
`regex_view.rs`, the entry points `convert_expr` / `convert_stmt` / ... are `view_expr` / `view_stmt` / ..., and the per-family
`convert(...)` functions are `view_node(...)` (a free `view` would read as the `Value::view` method to the walker check).
`scripts/frontend-debt.d/convert-lines.txt` is `view-lines.txt` with the same ceiling. No behaviour changes.
