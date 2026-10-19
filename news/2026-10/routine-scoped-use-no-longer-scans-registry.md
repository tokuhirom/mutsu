# A routine-scoped `use` no longer rescans the routine registry per call

`sub f { use Test; ... }` re-runs its import on every call and rolls it back
when the call returns. Each round trip cost time in proportion to the whole
routine registry (#9170): about 0.24 ms per call with 500 registered routines,
1.57 ms with 8000.

Three registry-wide passes per call are gone:

- The import and the rollback no longer drop the whole base-name key index
  (`invalidate_fn_resolution`). They evict only the base names whose keys
  they installed, removed or put back
  (`invalidate_fn_resolution_changing_keys`).
- An evicted plain base name is refilled from the qualified-name tail index,
  O(k), instead of a pass over the functions map. Before, the next lookup paid
  that pass.
- The rollback's filter over every registered routine is skipped when the
  scope's imports were its only writes to the routine table. Imports now log
  the keys they install (`ImportScopeSnapshot::installed_function_keys`), and
  a write counter on the registry (`Registry::function_writes`) proves nothing
  else wrote, so the pop judges just those keys.

For a program with 8000 routines and 4000 calls, the run went from 7.1 s to
2.2 s. `scripts/vm-complexity-check.sh 'routine-scoped use'` now measures a
flat ratio (0.93 for 2000 → 4000 routines). The first write in the body still
copies the shared routine table, so the issue stays open for that copy.
