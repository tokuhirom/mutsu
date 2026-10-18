# A block-scoped `use` costs less per run

Following on from the replay of a block-scoped `use` (#13906), the bookkeeping
around such a `use` and the calls to what it imports got cheaper. Measured
with callgrind on a release build, per call:

| Case | Before | After |
| --- | --- | --- |
| `sub f($x) { use OnlySubs; plus1($x) }` | 23.2k | 13.2k |
| `sub f($x) { plus1($x) }` with a top-level `use` (reference) | 9.4k | 9.4k |
| `{ use OnlySubs; ... plus1($_) }` (one call inside the block) | 11.4k | 5.4k |
| The registry alias's call (for comparison) | | 6.0k |

What changed:

- **The routine-alias tables are not copied.** A replay used to extend
  `imported_routine_aliases`, which copied the whole set because the import
  scope's snapshot shared it. Now the replay installs the set its first run
  produced, as long as the scope's exit has put back the table that run
  started from.
- **Import scopes no longer copy `imported_env_aliases`.** The env-alias table
  is copy-on-write, so opening an import scope no longer clones it. Before,
  this cost O(every alias the program imported).
- **The importer's type aliases are not copied either.** The scope now holds
  the copy-on-write `package_type_aliases` table instead of a clone of the
  importer package's aliases.
- **A lexically imported routine binds off the stack.** A call of such a
  routine with positional arguments now binds them in place, as the registry
  alias's cached call does. Before, it first drained them into an argument
  vector and decoded the call site's sources.
- **The exit scan for a module's `MAIN` is skipped.** It no longer reruns
  while the routine table is the version it last found clean.
- **Fewer per-exit allocations.** The routine-alias difference is computed
  only when the routine table changed. The per-scope key sets hash with
  FxHash, and restoring the env keys no longer allocates their names.
