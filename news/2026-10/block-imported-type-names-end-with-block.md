# A block's imported type names end with the block

A type a block-scoped `use` imports (`{ use M; Foo.new }` where `M` exports
`class Foo`) is no longer visible after the block. The import gave the
importing package a short-name alias for the type that nothing took back, so
`EVAL 'Foo'` after the block, and a top-level routine the block calls doing
`EVAL 'Foo'`, still found it; in rakudo neither does. The block's import scope
now restores the package's aliases when it closes (ADR-8751 stage 2). A closure
taken in the block keeps the type, as before.
