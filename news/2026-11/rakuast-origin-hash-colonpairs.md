# RakuAST origins of colonpairs in a hash composer

`.AST` now places the colonpairs of a hash composer, as rakudo 2026.11 does:
`{:2b, :!c, :d<x y>, :e(5), :z}` gives each `ColonPair::*` its range, its
value (`IntLiteral`, the word quote, the parentheses and what they hold) and
the comma list over them. The parser keeps a composer's entries as key/value
pairs, so the pairs are read back from the composer's text, entry by entry,
and placed only where the text spells the node exactly.

A `:key[...]` colonpair now places its `Circumfix::ArrayComposer` the way
`:key(...)` places its parentheses, and a `token word ($e)` whose signature
follows the name after a blank places that signature (#15780).
