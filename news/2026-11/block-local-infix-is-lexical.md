# A block's own `sub infix:<op>` no longer leaks into the routines it calls

A plain `sub infix:<**>` declared in a block (or a routine body) used to be
registered in the flat routine registry and to add the whole compilation unit
to the user-operator gate, so every routine the block called saw it for the
block's dynamic extent: `sub third { 3 ** -1 }; { sub infix:<**>(\a, \b) { 42 }; third() }`
gave `42`. Rakudo resolves an operator at its use site's lexical scope and
gives `0.333333`.

Such a declaration now binds in a synthetic lexical set of the block, the
machinery a block's `use` binds an imported operator in (ADR-8751 stage 2,
#17767). Every use inside the block reads the set: a binary use, a compound
assignment, `&infix:<**>`, a reduction, triangle reduction or hyper (which now
fold with the operator itself when it replaces the built-in, also for an
imported plain operator), a `.map` callback, a `where` predicate and an
`EVAL`. The block opens an import region that holds the set while it runs, and
the declaration rolls the routine registry back, so a routine written outside
the block keeps the built-in operator, by name too. A closure taken out of the
block keeps the operator (#17771).

A block's `multi` or `proto` of an operator still takes the registry path
(#18617).
