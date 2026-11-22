# `%h<p> := 1 => 2` keeps its `<p>` subscript in RakuAST

Binding a pair to a word-quoted subscript lost the written subscript:
`Q[%h<p> := 1 => 2].AST` gave a `Postcircumfix::HashIndex` over a `SemiList`
instead of rakudo's `Postcircumfix::LiteralHashIndex`. `=>` and `:=` share a
precedence tier, so the parser re-associates the bind to make the pair the
bound value, and that step rebuilt the subscript without its spelling. It now
keeps it, so the node, its `.DEPARSE` and its origins match rakudo (#18471).
