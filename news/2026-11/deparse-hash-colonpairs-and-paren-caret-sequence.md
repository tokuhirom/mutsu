# `.DEPARSE` keeps hash-literal colonpairs and parenthesised `^...` sequences

`{:2b, :1c}` and `%(:!b, :c)` were rendered as `{b => 2, c => 1}`: a hash
literal entry is a bare `(key, value)` and kept no pair node. The parser now
reads a colonpair entry with the ordinary colonpair parser and remembers the
form it was written as on the entry's value (`Spelling::ColonPairValue`), so
`.AST` shows `ColonPair::Number` / `::False` / `::Value` / `::Bracketed` again.

A left-exclusive sequence inside parentheses, `(1, 2 ^... 10)`, was rendered as
`((1, 2 ... 10).skip(1))`; both parenthesised sequence parsers now record the
written operator like the top-level one, matching raku's `(1, 2 ^... 10)`.
