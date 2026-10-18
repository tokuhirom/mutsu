# A `use` inside a routine body no longer re-imports on every call

ADR-8751 binds a block-scoped `use`'s routines in a lexical import set, but the
`use` still ran its whole import every time its block was entered: for
`sub f($x) { use OnlySubs; plus1($x) }` every call walked `import_module` and
then, when the block's import scope closed, flushed every name-keyed call cache
in the process. Three costs of a block-scoped `use` are cut (#13906; callgrind,
release build):

- **The import is replayed.** The first run of such a `use` is checked for
  effects beyond its set: changes to the routine, proto, class and type-alias
  tables, a pragma, an env alias other than the module-merge grant, or a
  `sub EXPORT` hook's value. When there are none, what it wrote is recorded, and
  later runs replay just that (`runtime/lexical_import_replay.rs`). An import
  scope that left the routine tables untouched no longer flushes the call caches
  on exit, and no longer copies the routine table to restore nothing. The
  example call went from 98.7k to 32.9k instructions.
- **A lexically imported routine takes the positional-light call.** Such a call
  went through the general named entry every time. It now takes the same light
  path the registry alias's call did (23k → 11.6k instructions per call, against
  10.2k for the registry alias).
- **Built-in operators skip the import-set probe.** In a block that `use`s a
  module, each built-in operator asked the open import sets for a user
  candidate (~2k instructions per op). It no longer asks while no set binds an
  operator.
