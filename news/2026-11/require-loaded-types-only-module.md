# `require` of an already loaded types-only module no longer reloads it

`builtin_require` decided that a loaded module had been "cleaned up by an import scope's
pop" by looking for a routine named `Module::...` in the registry. A module that declares
only types (a role, a class -- Monad's `Monad::Maybe`) has no such routine, so every
`require` of it dropped it from `loaded_modules` and parsed and ran it again (~14 ms per
call, 60x rakudo; ~64% of Monad's `t/14_concurrent_types.t` single-thread profile, which
requires `Monad::Maybe` per conversion). It now uses `module_still_registered`, which also
accepts the known package, as `CompUnit::Repository::*.need` already did (#17228).
