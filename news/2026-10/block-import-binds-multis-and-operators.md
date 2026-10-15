# A block's `use` binds imported multis and operators lexically

A `use` inside a block now binds the multi families and binary operators it
imports in the block's lexical import set (ADR-8751 stage 2), not only the
plain routines. A closure made in the block keeps calling them after the
block exited:

```raku
sub mk { use M; -> { f('s') ~ (3 +%+ 4) } }   # M exports `multi f` and `infix:<+%+>`
mk()();
```

That died with "Unknown function" or "Two terms in a row", or reached the
importing unit's own `multi f` when it had one. An imported family's
`&f` value also stopped reaching the importer's same-named family. A
callee written outside the block no longer sees an imported multi family,
and a `multi` the block declares itself still merges with the import.
