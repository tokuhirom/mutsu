# `%EXPORT<&name>` keys and `proto method` expressions parse

Working Object::Delayed from the ecosystem ledger: the module no longer fails to load.
A module exporting through `sub EXPORT { %EXPORT }` now has its literal `%EXPORT<&name>`
binding keys registered as routines for the importer's parse (so `slack { ... }` is a
listop call), and `proto method NAME(...) {*}` is accepted in expression position
(Object::Trampoline). The remaining gap (`^find_method` catch-all dispatch, the value of a
lexical proto method) is tracked in #10804.
