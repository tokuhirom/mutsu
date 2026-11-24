# A re-exported `EXPORT::ALL::` stash keeps the family's proto

A `sub EXPORT` that hands out another module's `EXPORT::ALL::` stash now gives
the importer a multi dispatcher that runs the family's `proto`, even when only
the `multi` candidates carry `is export`. The stash's `&name` was built from the
candidates' export aliases alone, so the proto body never ran and the call went
straight to a candidate. Red's `use Red` re-exports `Red::Do::EXPORT::ALL::`,
and its unexported `proto red-do` validates and binds the row-security context
before `{*}`, so Red 0.2.17's row-security tests saw no context (#19175).
