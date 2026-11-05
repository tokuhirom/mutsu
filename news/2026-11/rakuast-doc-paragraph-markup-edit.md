# RakuAST Doc::Paragraph and Doc::Markup builders

`RakuAST::Doc::Paragraph` gains `from-string` (a class method that answers the
text), `leading-whitespace` (the whitespace at the start of the first atom) and
the in-place `add-atom`. `RakuAST::Doc::Markup` gains `add-atom` and
`add-meta` (they answer what they add), `set-atoms`, `set-meta`, `set-opener`
and `set-closer` (they answer `Nil`; a delimiter must be a `Str`) and
`flatten` (the text between the delimiters, nested codes spelled out). All
measured against rakudo 2026.09. `check-meta` and `splat-letterless`, which
report problems rather than change the node, and the `Doc::Block` builders
remain.
