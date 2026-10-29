# The declaration-under-a-modifier split lives in `rakuast`

ADR-15334 (reading B), statement-modifier declarations. The lowering of `my $x = 5 if COND`, `unless`,
`with` and `without` -- the declaration takes effect unconditionally while only its initializer is gated, for
scalar, array, hash, list-binding and signature declarations -- moved verbatim from
`src/parser/stmt/modifier_decl_split.rs` to `src/rakuast/decl_modifier_split.rs`, next to its only caller
`rakuast::modifier_lower`. The parser no longer holds a `SyntheticBlock` producer for it, and the
`parser::conditional_declaration` / `topical_declaration` re-exports are gone; the two parse helpers it needs
(`source_body::check`, `default_decl_expr`) are exported from the parser instead.

`t/rakuast/rakuast-stage2-decl-modifier-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` mean the same thing for these forms; it passes identically under `raku`. The
`synthetic-block-sites` ceiling drops 44 to 36 and `internal-name-literals` 59 to 58.
