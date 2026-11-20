# RakuAST: colonpair words in `<< >>` quotes are `QuoteWordsAtom`s

A colonpair written as a word of a `<< >>` / `« »` quote
(`<< a :b<c> d >>`, `<< :!b :c("x y") >>`) is now a `QuoteWordsAtom` over its
`ColonPair` in the RakuAST view, and the text around it — blanks and plain
words — stays `StrLiteral` segments, as in rakudo 2026.11; mutsu showed the
whole body as one text segment. An enum body with such atoms
(`our Str enum InlineUnits << :ms<time> :s<time> >>`) keeps its written
spelling the same way instead of re-rendering each pair (`:ms("time")`), so
`.DEPARSE` writes it back as written (#18299). Execution was already right;
`EVAL` of the tree builds the same values.
