# The parameter binds of pointy blocks live in `rakuast`

ADR-15334 (reading B), pointy-block family. The head statements that bind a pointy block's parameter to the
value a control statement tested or topicalized (`given EXPR -> PARAM`, `with` / `if COND -> PARAM`: the
aliasing and `is copy` binds, sigilless terms, native and typed parameters, sub-signature unpacking, the
`given` autothreading call) and the `while` / `until` / `repeat` loop parameter expansion moved out of
`src/parser/stmt/control*` into `src/rakuast/{pointy_bind,loop_bind}.rs`. The parser's anonymous-sub site
and `rakuast::{conditional_lower,with_branch,if_branch}` call them there; the `parser::{given_pointy_body,
loop_pointy_clause, repeat_param_clause}` re-exports are gone.

`t/rakuast/rakuast-stage2-pointy-binds-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` mean the same thing for these forms (declared inside the unit and in the caller); it passes
identically under `raku`. The `synthetic-block-sites` ceiling drops 54 to 47: the `SyntheticBlock` producers
of the pointy binds are no longer in the parser.
