# The expansion of signature declarations lives in `rakuast`

ADR-15334 (reading B), signature-declaration family. The expansion of `my ($a, @b) = …`, `my (…) := …`
and the bare form into ordinary declarations reading a staging temporary moved out of
`src/parser/stmt/decl/destructure/` into `src/rakuast/signature_expand/` (the expansion, the bind-arity
checks, the binding result and the named-element expansion). The parser keeps the parse functions and
`settle`, the finalizer of what the parse produces; `rakuast::{signature_decl, modifier_lower,
conditional_lower}` and the parser's modifier/declaration splitters call the one expansion, so the
`crate::parser::{pending_declaration, lower_pending_declaration, signature_decl_expansion}` re-exports are
gone.

`t/rakuast/rakuast-stage2-signature-decl-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` mean the same thing for these forms (declared inside the unit and in the caller); it passes
identically under `raku`. A nested group (`my ($a, ($b, $c)) = 1, (2, 3)`) binds wrongly before and after
this change: [#15452](https://github.com/tokuhirom/mutsu/issues/15452). The `synthetic-block-sites` ceiling
drops 58 to 55 and `internal-name-literals` 68 to 66.
