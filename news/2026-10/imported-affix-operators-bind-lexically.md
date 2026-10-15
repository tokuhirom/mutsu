# Imported prefix, postfix and circumfix operators are block-scoped

A `use` inside a block now binds the user `prefix`, `postfix` and `circumfix`
operators it imports in that block's lexical import set, as it already did for
plain routines, multis and infix operators (ADR-8751 stage 2, slice 3). A sub
written outside the block, called from inside it, no longer sees the imported
operator; it gets the unit's own operator of that name, as in Rakudo. Closures
made in the block keep the import after the block exits.

User candidates of built-in operators (`multi prefix:<->`) and `postcircumfix`
operators still go through the registry.
