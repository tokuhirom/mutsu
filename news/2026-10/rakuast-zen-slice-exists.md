# RakuAST round-trips `@a[]:exists` zen slices

`@a[]:exists:delete` and `%h{}:exists` keep the parser's zen-slice form (it
does so only when `:exists` directly follows the empty subscript), which the
adverb read-back did not accept. The converter now renders it as an
`ArrayIndex`/`HashIndex` with an empty `SemiList` and the adverbs as
colonpairs, and lowering rebuilds the zen slice when `:exists` leads.
ADR-10723 section 6.54.
