# The `.=` expansion lives in `rakuast`

ADR-15334 (reading B), dotty-assignment family. The expansion of `X .= meth` into the assignment of `X.meth`
back to `X`, evaluated once, for each shape of target (a simple variable, an element through a hidden index
temporary, a chained `.=`, a grouped or `.self` target, an inline declaration, a sigilless term, the topic)
moved out of `src/parser/expr/postfix/dot_assign.rs` and `simple_expr_stmt/core.rs` into
`src/rakuast/dotty_assign.rs`, next to the `.=` marker it already used (`compound::expansion`). The parser
keeps `parse_dot_assign` and the other parse functions; `rakuast::{compound::lower, lower}` and the parser's
assignment sites call the one expansion, so the `parser::{wrap_dot_assign_with_terms, retarget_dotty_call,
topic_dot_assign}` re-exports are gone.

`t/rakuast/rakuast-stage2-dotty-assign-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` mean the same thing for these forms (declared inside the unit and in the caller); it passes
identically under `raku`. The `internal-name-literals` ceiling drops 66 to 61.
