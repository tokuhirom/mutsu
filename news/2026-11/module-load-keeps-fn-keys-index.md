# A module load keeps the routine key index instead of rebuilding it

`use Test; ok 1;` spends 36.26M Ir loading the module (profiling build,
callgrind, minus an empty script). Part of that went to the routine
base-name key index. Ten wholesale `invalidate_fn_resolution` calls during the
load dropped the index, and the next lookups paid cold misses and full
registry passes to rebuild it. Those calls were the load epilogue, the export
step, the import renames and removals, every `proto`, and the wrap sync.

Those sites now evict only the keys whose presence they changed:

- a direct list where the site knows the keys (a `proto`'s removed
  candidates, the import renames and removals);
- otherwise a key-set diff across the span (`invalidate_fn_resolution_since`);
- no keys at all for the wrap sync, which moves no registry key.

The index stays complete across the load.

A complete index answers a dirty base name from the qualified-name tail index.
The tail index's first read in a process folds in every symbol interned so
far, which cost more than the registry pass it replaced. A cold tail index now
defers to that pass.

The `use Test` load drops from 36.26M to 34.14M Ir (−5.8%), towards #12026's
close condition of 20.6M.
