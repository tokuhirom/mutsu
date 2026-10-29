# The compound-assignment lowering lives in `rakuast`

ADR-15334 slice 2 (reading B). The expansion of `OP=`, `.=`, the reverse,
bracketed and set metaoperator forms and the short-circuit scalar forms
(`src/parser/stmt/assign/{compound_expr,compound_lower}.rs`, the expansion half of
`op.rs` and `set_compound.rs`, and the `CompoundAssignOp` enum) moved to
`src/rakuast/compound/`. The parser keeps only the parse functions and calls
the one lowering; `EVAL` of a `.AST`, the `*`-curry rewrite and the
declaration forms call it directly instead of through the string-error shims
`parser::{lower_compound_expression, lower_compound_statement,
lower_dotty_*, is_compound_assign_op, is_set_compound_op}`, which are gone.
The two separate temporary counters (`assign.rs`, `simple.rs`) are one.

`t/rakuast/rakuast-stage2-compound-assign-paths.t` pins that a file, an `EVAL`
string and an `EVAL` of the string's `.AST` agree for the family, with the names
declared inside the unit and in the caller.
