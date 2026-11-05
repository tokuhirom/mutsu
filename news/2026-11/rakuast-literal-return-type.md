# RakuAST Literal.return-type

`return-type` on the literal nodes answers what rakudo 2026.09 does instead of
`Mu`: a `Literal` is the type of the value it holds (`IntLiteral` `Int`,
`NumLiteral` `Num`, `RatLiteral` `Rat`, `ComplexLiteral` `Complex`,
`StrLiteral` `Str`, `VersionLiteral` `Version`), and `QuotedString` and
`QuoteWordsAtom` are `Str`. A node that declares no type still answers `Mu`.
