# `CALLERS::<&name>` looks in the nearest caller's own lexical scope

`::CALLERS::<&marker>` answered `True` from a routine of a module unit that
imported nothing, as long as the program imported `&marker` through a `sub EXPORT`
hook. Two things made it too broad:

- the caller env was read through the frames that called it, so the importer's
  env alias was found. A routine is a lexical, so it is now read where the
  caller's own scope binds it (following that frame's static link);
- the scope fallback (`caller_scope_routine`) searched every calling routine and
  then the program mainline. Only the nearest calling routine's scope counts now
  (the mainline only when no routine called), as in Rakudo.

`t/modules/callers-routine-lookup-ignores-unimported-module-unit.t` pins the
case; the existing `callers-routine-lookup-sees-import-and-file-scope.t` stays green.
