# Normalization-form quotes cross the RakuAST boundary

`q:nfc'ẛ̣'` is an `NFC` value, and `q:nfd`, `q:nfkc` and `q:nfkd` give the
other three `Uni` subtypes. The RakuAST converter refused these literals, so
`roast/S15-string-types/NF-types.t` and `NFK-types.t` could not round-trip
under `MUTSU_RAKUAST=1`.

Rakudo 2026.09 offers no shape to copy. Its parser rejects the adverbs
("Unrecognized adverb: :nfc"), and its `QuotedString` refuses an unknown
processor. mutsu therefore picked the shape the RakuAST model already uses
for a quote adverb that post-processes its text: a processor, as `:w` is
`words` and `:v` is `val`.

```raku
say Q[q:nfc'abc'].AST.statements[0].expression;
# RakuAST::QuotedString.new(
#   processors => ("nfc",),
#   segments   => (
#     RakuAST::StrLiteral.new("abc"),
#   )
# )
```

`EVAL` of that node normalizes the text again and gives the same `Uni`
value as the quote. No module that uses `.AST` can be affected: rakudo
cannot parse the source these nodes come from.
