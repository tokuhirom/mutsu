# `use` inside a `do` block stays in the block; `CORE::.EXISTS-KEY('&name')` sees core routines

`head-skip-tail`'s `t/01-basic.rakutest` imports Test through a `do` block
(`BEGIN my (&plan, &ok) = do { use Test; (&plan, &ok) }`) and then loads a module that
declares `my proto sub skip`; the module died with "Redeclaration of routine 'skip'":

- Test's `skip`, imported by the `use` in the `do` block, stayed in the routine registry
  after the block. The compiler only snapshotted/restored the registry around a `do`
  body when it contained a `sub` declaration; a `use` (which imports routines) now
  counts too, as it does for any routine-declaring body.
- `CORE::.EXISTS-KEY('&head')`, which that module's `sub EXPORT` asks before exporting
  its own `head`/`skip`/`tail`, answered False for every core routine because the CORE
  stash is not materialized. It now finds core routines the way `CORE::<&name>` does,
  and a routine the program declared itself is still not a core symbol.

`head-skip-tail`'s test goes from 0 to 6 ok, as `raku`.
