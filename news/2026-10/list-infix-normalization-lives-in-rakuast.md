# List-infix normalization and semicolon sections live in `rakuast`

ADR-15334 (reading B), list family. The normalization of a parenthesized list or a listop argument list
(`Z` / `X` meta-ops lifted across the comma, `minmax`, a feed looser than the comma, the Whatever-curry of a
standalone `X` / `Z`, chained zip metas and sequence waypoints) moved from
`src/parser/primary/container/meta_ops.rs` to `src/rakuast/list_infix.rs`, and the executable shape of a
semicolon-sectioned list (`[a, b; c]`, `(a, b; c)`) from `container/{array,paren}.rs` to
`src/rakuast/sections.rs`. The parser keeps the parse functions (the sequence parsers, inline modifiers) and
calls the one implementation; `rakuast::lower` calls it directly, so `parser::{lower_array_sections,
lower_paren_sections}` are gone.

`t/rakuast/rakuast-stage2-list-infix-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` mean the same thing for these forms (declared inside the unit and in the caller); it passes
identically under `raku`. The three `ast-walkers` baseline entries moved with their code.
