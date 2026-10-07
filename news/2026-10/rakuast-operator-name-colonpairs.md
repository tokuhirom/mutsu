# RakuAST: declared operator names carry their symbol as colonpairs

`sub infix:<foo>` now has the name rakudo gives it,
`Name.from-identifier("infix", colonpairs => (QuotedString<words val>("foo"),))`,
instead of one `infix:<foo>` identifier string. The same holds for `prefix`,
`postfix`, `circumfix`, `postcircumfix`, `term` and `trait_mod`, and for a
package-qualified spelling (`from-identifier-parts`). Lowering reads the adverb
back into the original name, so `EVAL` of such an AST still declares the
operator, and `RakuAST::Name.colonpairs` is now an accessor (empty for a plain
name). The `["foo"]`, `sym<foo>` and `<<foo>>` spellings are other node shapes
and stay as before.
