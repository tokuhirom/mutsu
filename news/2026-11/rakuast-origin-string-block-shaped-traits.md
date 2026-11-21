# RakuAST: origins in interpolated blocks and after a variable's shape

Two `.origin` gaps closed (#15780). The statements of a block interpolated
in a string that the parser left without a range (`"{:a(7)}"`, `"{:a}"`) are
now read again from the text between the braces, as a parenthesized
statement already was; the block placement moves to
`string_block_origins.rs`. And the traits of a shaped variable
(`my %h{Str} of Int`, `my @a[3] of Int`) are read after the shape, so the
`Trait::Of` / `Trait::Is` and its type are placed.
