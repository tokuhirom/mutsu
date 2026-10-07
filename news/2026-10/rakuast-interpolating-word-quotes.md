# RakuAST: `.AST` of interpolating word quotes

`.AST` of `<<a $b>>`, `«a "b c"»`, `qqww/a $b "c d"/` and `qw:v/1 2/` used to be refused. It now
matches rakudo: a `QuotedString` with the `quotewords` (and `val`) processors whose segments are
the unquoted runs, the interpolated terms and one new `RakuAST::QuoteWordsAtom` per quoted word.
Hand-built nodes of that shape EVAL to the same word list as the source.
