# `UNIT::` lists a unit's own `my` classes and roles

highlighter's `sub EXPORT` exports its `my role Type` by filtering its own
`UNIT::`: `UNIT::.grep: { .key eq 'Type' || .key.starts-with('&') }`. mutsu
builds `UNIT::` from the running environment and the routine registry, and a
lexical type lives in neither, so `Type` was never exported and every test
that wrote `"bar" but Type<words>` died with `Unknown function: Type`.

`UNIT::` now also lists the top-level `my`-scoped classes and roles of the
compilation unit that is running, found through the unit that declared them.
Roles now record their declaring unit the way classes already did, so a
module's lexical role is listed in the module's own `UNIT::` and never in an
importer's.

highlighter's t/01, t/03 and t/04 now pass under mutsu; t/05 still needs
#11246 (a second block-scoped `use` of the same module loses its exported
multis from `UNIT::`).
