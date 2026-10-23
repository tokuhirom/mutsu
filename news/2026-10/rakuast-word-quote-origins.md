# RakuAST word quotes and literal hash subscripts carry their origins

`.AST` now gives the following nodes the ranges Rakudo does:

- the text segment of a word quote written `<a b>`, `<<a b>>` or `«a b»`;
- the `<...>` / `<<...>>` / `«...»` subscript of `%h<a b>` (a
  `Postcircumfix::LiteralHashIndex`), its word quote and the text in it;
- a single `:name` adverb after the closing bracket (`%h<a>:exists`).

A word quote keeps the raw text between its brackets, so the range is
attached only when the literal equals the text written there. A key padded
with spaces (`%h< a >`), which mutsu holds trimmed, stays without a range:
exact or absent.
