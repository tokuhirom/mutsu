# A block's `use` no longer leaks into the routines it calls

`sub other { g() }; { use M; other() }` called `M`'s `g`: a `use` in a block
installed its routines into the routine registry while the block ran, so a
routine written outside the block, called from inside it, saw them too.
Rakudo binds an import in the block's own lexical scope only, and `other`
calls the unit's own `g`.

A plain routine a block-scoped `use` imports now lives only in the block's
lexical import set (ADR-8751 stage 1, slice 2). Every reference the block
itself contains reads it: a bareword call, `&g`, `&g(...)`, a by-name
`::('&g')` and an `EVAL` in the block. Multis, protos and operators still go
through the registry until stage 2.
