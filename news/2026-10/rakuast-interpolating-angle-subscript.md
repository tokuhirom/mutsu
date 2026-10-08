# `%h<<$key>>` and `%h«a "b c"»` cross the RakuAST boundary

An interpolating angle subscript was parsed into the word-list machinery's internal call
(`__mutsu_qw_result(..)`), which `.AST` refused, so a unit containing `%h<<$key>>` or
`%h«a $k»` could not round-trip under `MUTSU_RAKUAST=1`.

Rakudo 2026.09 renders it as `Postcircumfix::LiteralHashIndex(index => QuotedString(processors
=> <quotewords val>, segments => ...))` over the text as written. The parser now wraps the
index in the term-spelling record (`Spelling::WordQuote` for plain words, `InterpolatingWords`
otherwise) and marks the subscript `IndexSpelling::Angle`; `convert` renders the recorded quote
through the existing `word_quote` machinery, and `lower` already rebuilds the word list from
such a `QuotedString`. Assignment (`%h<<$k>> = 1`) and the `:exists` / `:delete` adverbs go
through the same node.
