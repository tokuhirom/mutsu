# A routine with an inner `my sub` no longer resets the whole program's call caches on every call

[#9073](https://github.com/tokuhirom/mutsu/issues/9073). JSON::Fast's
`unjsonify-string` declares `my sub fetch-codepoint`, and mutsu installs a
routine-local sub into the program-global function registry on every entry and
takes it away again on exit. #8314 had already made the registry's version
stamp name its *content*, so the stamp alternates between two values instead of
counting up. But most of what hangs off that stamp still behaved as if it
counted, and several other costs of the excursion were O(registry) per call.

## What each call used to pay, and what changed

- **The call caches emptied twice per call.** The positional and named
  light-call caches, the OTF call cache and the three `func_multi_*` resolution
  memos kept one generation per *table* and `clear()`ed on any mismatch. Each of
  them is now tagged per entry, like `fn_resolve_cache` already was, so an
  answer computed outside the routine is still there when control comes back.
  The direct-mapped `call_ic` slots carry no generation of their own; a
  generation change now retires them by bumping their epoch and leaves the
  name-keyed entries that refill them alone.
- **`GenCache` holds two generations per key, not one.** The excursion is two
  alternating generations, and the same key (`return`, most obviously) is
  probed on both sides of it. With one slot, that key was recomputed on every
  switch. A third generation evicts the older slot, so the table is still
  bounded by the number of keys.
- **The re-install copied the whole functions map.** It went through
  `Arc::make_mut` on a map the scope snapshot shares. `FunctionTableTransitions`
  now keeps the resulting table of any transition it has seen twice (capped at
  64 tables) and hands that `Arc` back. The content is named by its version and
  a shared `Arc` is never written in place, so the reuse is sound for the same
  reason the version reuse is.
- **The restore diffed two whole maps.** The transition memo now also records
  `version after -> (version before, key)`, and the restore walks those links
  back to the snapshot's version to learn the keys it is giving back. Any other
  write in between leaves no link, and the restore then falls back to the diff.
- **The restore dropped the lexical type-name index.** Every `registry_mut()`
  drops `Registry::has_lexical_type_key_for`'s index, and the next bare
  type-name lookup rebuilt it from every key of the four type maps. This was
  ~100K instructions per call, the largest single item. The install and the
  restore write only routine and token tables, so they now go through a new
  `RegistryWriteGuard::routine_tables_mut`, which leaves the index in place.
- **Restoring an unchanged `proto_subs` set still bumped `proto_gen`.** It now
  returns early when the snapshot `Arc` is the one already installed.

## Measured

`tmp/rep9073.raku` from the issue (`from-json` of an array of `"a/b"`
strings). Instruction counts are from callgrind on `--profile profiling`
builds, differenced between 100 and 600 strings, each binary warmed first:

| | per string |
| --- | ---: |
| `main` (release, callgrind) | 872K |
| this change | 715K |
| `fetch-codepoint` hoisted out of the routine (scratch copy of the module) | 619K |

So the part of the per-string cost that the inner sub is responsible for drops
from ~253K to ~96K instructions. Under `MUTSU_VM_STATS=1`, `program-table-cow`
clones go from one per string to zero.

## What is left

Most of the remaining ~96K comes from the call of `unjsonify-string` itself.
Any body that declares a sub has `has_inner_subs` set, which keeps it off the
light and OTF call paths, and the slow dispatch path then resolves the callee
three times per call. That is filed as
[#9081](https://github.com/tokuhirom/mutsu/issues/9081), together with the
structural fix #9073 originally proposed: a frame-local binding, with no
registry write at all. While writing the regression test, two same-name
shadowing bugs turned up that are already present on `main`. They are filed as
[#9080](https://github.com/tokuhirom/mutsu/issues/9080).

Pinned by `t/routines/dispatch/inner-sub-call-cache-survival.t` and by unit
tests in `gen_cache.rs`, `function_table.rs` and `accessors_state.rs`.
