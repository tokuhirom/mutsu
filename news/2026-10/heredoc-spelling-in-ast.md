# `.AST` of a heredoc keeps its terminator line

`q:to/END/`, `qq:to/END/` and `Q:to/END/` now render in `Str.AST` as rakudo's
`RakuAST::Heredoc.new(segments => (...), stop => "    END\n")` instead of a plain
`QuotedString`. The terminator line is carried by `Spelling::Heredoc` on `Expr::Spelled`
(ADR-12199, slice S2), built only by parses that keep spellings, so execution never sees it.
A hand-built `RakuAST::Heredoc` goes through `EVAL`. Checked by
`t/rakuast/rakuast-heredoc-spelling.t`, which also passes under raku.
