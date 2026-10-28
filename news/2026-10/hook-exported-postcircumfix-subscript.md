# A `sub EXPORT` hook's `postcircumfix:<[ ]>` family reaches the subscript

A module that hands its `multi postcircumfix:<[ ]>` candidates out through a
`sub EXPORT` hook (`'&postcircumfix:<[ ]>' => ...`, Red's `Red::Operators`
re-export) keeps them private to the exporter, so neither the registry nor the
block-scoped import machinery held them and `$obj[$ast-index]` silently fell
through to the built-in subscript (`Nil`/`Any`). The plain subscript now tries
the family the importing unit was handed, as the infix operators already did,
and declines to the built-in subscript when no candidate accepts the operands.
This makes Red's JSON-column index reads and writes (`.data<a><b>[.item-index]`)
reach `Red::ColumnMethods.AT-POS`.
