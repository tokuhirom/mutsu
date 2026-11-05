# RakuAST Doc::Block.from-alias and from-config

`RakuAST::Doc::Block.from-alias(:type, :$lemma, :@paragraphs, ...)` builds a
block whose paragraphs are the lemma and then the text of the given paragraphs,
each element ending in a newline (an empty text when there are none; a missing
lemma is the `Any` type object), and `from-config(:type, :$key)` builds one
whose only paragraph is the key. Both forward the rest to `new`, as on rakudo
2026.09. `allowed-markup` (the type objects `Doc::Block::OK` / `NOK`) and
`visual-table` need a new type and wait for a design choice; a constructor that
accepts an unknown named argument is filed as #16771.
