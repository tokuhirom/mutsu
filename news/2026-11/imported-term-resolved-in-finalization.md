# Imported terms are resolved where finalization finds them

A word the parse spelled as an imported term (`Spelling::ImportedTerm`: the native types
`uint16`/`int32`/`num64` that `use NativeCall` brings in, imported constants) is resolved by the
classifier in `finalize_unit` (`bareword::imported_term_expr`) instead of being converted to a
RakuAST node and lowered back to the same bareword (ADR-15760 slice 2). A decorated or otherwise
classified word still goes through the conversion.
