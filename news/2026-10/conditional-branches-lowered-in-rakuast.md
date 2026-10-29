# The branch lowering of `if` / `unless` / `with` lives in `rakuast`

ADR-15334 (reading B), conditional-branch family. The expansion of a pointy `if COND -> $x { }`, of
`else -> $x { }`, of an `elsif` / `orwith` chain folded into nested `Stmt::If`s, of `unless` as an `if`
over a negated condition, and of the `with` topic / parameter binds moved out of `src/parser/stmt/control/`
into `src/rakuast/{if_branch,with_branch}.rs`. `rakuast::conditional_lower` calls them directly instead of
through `crate::parser`, so there is one lowering and the parser only parses the clauses.

`t/rakuast/rakuast-stage2-conditional-branches-paths.t` pins that a file, an `EVAL` string and an `EVAL` of
the string's `.AST` mean the same thing for these forms (names declared inside the unit and in the caller);
it passes identically under `raku`. The `synthetic-block-sites` ceiling drops 59 to 58.
