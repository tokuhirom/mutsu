# A package-less module's own multi/proto family survives a block-scoped `use`

A module with no `unit` declarator that exports a `multi` (or `proto` + multis) and reads its own
routines through `UNIT::` in `sub EXPORT` lost the family once the first block-scoped `use` ended:
the second `use` in a sibling block re-ran `EXPORT` against an empty `UNIT::` (#11246). The loader
now remembers each package-less module's own candidates and puts them back for the duration of the
re-run `EXPORT`, then takes them out again, so neither block keeps the module's lexicals.
