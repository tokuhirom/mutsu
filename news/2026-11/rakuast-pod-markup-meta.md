# RakuAST: Pod formatting codes with metadata

`X<a|b;c>`, `L<text|url>`, `D<..|..>` (and `A`, `F`, `M`, `P`) in a Pod paragraph are
`RakuAST::Doc::Markup` nodes of `.AST` with the text after the first `|` as a one-string
`meta` list, shaped as rakudo shapes them (`atoms` omitted when empty). The letters without
metadata keep a `|` in their text. `E<...>` entities are still left out of the tree (#14880).
