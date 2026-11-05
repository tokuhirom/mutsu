# Regexes with a dynamic :p/:c offset are carried through finalization

`m:p($pos)/.../` and `m:c($pos)/.../` (`Expr::MatchRegexDynamicAdverbs`) are kept by
`finalize_unit` as the parser built them instead of being converted to a quoted-regex node and
lowered back (ADR-15760 slice 2, #8033). The adverb expressions are finalized by the walk, and the
regex value's own tree is finalized when it holds code, as for the other regex families.
`t/rakuast/rakuast-regex-dynamic-adverbs-finalize.t` pins the semantics.
