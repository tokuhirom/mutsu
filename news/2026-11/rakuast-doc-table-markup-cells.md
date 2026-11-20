# RakuAST Pod tables: formatting-code cells, NBSP blanks, more config values

A Pod `table` cell that holds a formatting code (`x | y Z<a comment>`) is now a
`RakuAST::Doc::Paragraph`, as rakudo's `Paragraph.from-string` makes it; plain
cells stay strings. As in rakudo, the first divider stringifies the rows above
it before merging them, and a later row stops merging at its first paragraph
cell. Tables written with non-ASCII blanks (the NBSP-padded operator table of
`S26-documentation/07a-tables.t`) are read by characters instead of being left
out, and a non-space `Zs` blank counts as a blank around a `|` / `+` divider.

A Pod block's configuration also takes `:key[...]` and `:key{...}` values and
the numbered form `:034foo` (`:foo(34)`). With these, the `.AST` of
`roast/S26-documentation/07a-tables.t` and `09-configuration.t` matches rakudo
2026.11 node for node: 25 of the 27 S26 files now do (#14880).
