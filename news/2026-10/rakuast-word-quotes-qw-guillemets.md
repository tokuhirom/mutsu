# RakuAST: `.AST` renders `qw`, `qww`, `qqww`, `q:w`, `«…»` and `<<…>>`

`.AST` used to refuse every word quote other than `<…>` ("desugared construct
(internal name `__mutsu_word_list`)"). These now come out as rakudo's one
`QuotedString` over the raw text: `processors => ("words",)` for `qw/Qw/q:w`,
`("quotewords",)` for `qww`/`qqww`/`qq:ww`, and `<quotewords val>` for `«a b»`
and `<<a b>>`. The converter reads the new `Spelling::WordQuote` carrier
(ADR-12199 section 6.1, built only by spelling-keeping parses), and the lowering
turns a hand-built `QuotedString` with any of those processor lists back into
the same word list through the parser's own `word_quote_expr`, so `EVAL` agrees.

Word quotes that interpolate or quote a word (`<<a $b>>`, `«a "b c"»`) and the
`:v` variants are still refused: their segments (`Var::Lexical`,
`QuoteWordsAtom`) are not carried yet.
