# The `will` trait lowering lives in `rakuast`

ADR-15334 (reading B), `will` family. A declaration with `will <phaser> { ... }` traits is now always the typed
`ParsedStmt::WillDecl` node (it now carries the declared variable's name), including the forms where the parser
holds the declaration as something other than a plain `VarDecl` (a feed, a chained or rewritten assignment).
`parser::wrap_with_will_phasers` -- the `SyntheticBlock` producer the parser called directly for those forms --
moved to `src/rakuast/will_decl.rs`, the one lowering, and the parser re-export is gone.

`t/rakuast/rakuast-stage2-will-decl-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the string's
`.AST` mean the same thing for these forms; it passes identically under `raku`. The `synthetic-block-sites`
ceiling drops 36 to 35.
