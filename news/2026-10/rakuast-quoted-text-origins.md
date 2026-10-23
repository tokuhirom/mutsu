# RakuAST quoted text and heredoc bodies carry their origin

`.AST` now gives the `StrLiteral` inside a quoted string the range Rakudo does:
the text between the quote's delimiters, escapes as written (`"a\tb"`), past a
`q` / `qq` / `Q` and its adverbs. A heredoc's `StrLiteral` covers its body
lines, so a `visit-children` walker such as `Code::Coverable` sees the line a
heredoc body starts on.

The heredoc's body range is recorded by the parser in its `Spelling::Heredoc`
next to the terminator line. A quoted string's inner range is read off the
quote's own source text once the quote has its origin, and only for a quote
with a single text segment and no word processors, so it is exact or absent.
