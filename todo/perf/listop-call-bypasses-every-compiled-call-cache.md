# A listop call (`ExecCallPairs`) reaches none of the compiled-call caches

`ok 1, "x"` and `is $got, $expected, "..."` — the shape every roast assertion is
written in — compile to `OpCode::ExecCallPairs`, not to `CallFunc`. That opcode
has only three arms (`src/vm/vm_call_exec_ops.rs`, `exec_exec_call_pairs_op`):

1. `find_compiled_function(compiled_fns, name, args)` — the *caller's* compiled
   table, which an imported module sub is never in;
2. `try_native_function` — a Rust builtin;
3. the **carrier fallback**: `snapshot_carrier_overwritable_env(code)`,
   `begin_carrier`, `exec_call_pairs_values`, `end_carrier`,
   `writeback_carrier_writes`, `carrier_writeback_changed_aggregates`.

`CallFunc`'s three name-keyed fast caches — `pos_light_cache`,
`light_call_cache`, `otf_call_cache` — are all in `exec_call_func_op` and none
of them is consulted here. So **every assertion of the vendored upstream
`Test`, in every roast file, takes the carrier path**, paying a whole-frame env
snapshot and a writeback diff per call, where the same sub called as
`ok(1, "x")` from an expression takes the cached compiled path.

Verified with gdb on `MUTSU_REAL_TEST=1 tmp/probe-is2.raku` (three `is` calls):
`CARRIER-FALLBACK "is"` fires three times, once per call.

## Two separate gaps

**(a) `ExecCallPairs` has no OTF cache at all.** Even a *non*-multi imported sub
called in listop form misses `otf_call_cache`, which
`news/2026-09/imported-module-sub-reaches-the-cached-dispatch.md` added for the
`CallFunc` path only. Wiring the same cache in is the smaller half of the work;
the care needed is in the named/`Pair` argument handling this opcode exists for.

**(b) `otf_call_cache` skips multi names by construction.** Its guard is
`!self.has_multi_candidates_cached_sym(name_sym)`, because the entry is keyed on
the name alone and a multi's winner depends on the argument types. That was
unavoidable when the winner had to be re-derived per call, but it no longer is:
`func_multi_resolve_cache` now resolves `ok`/`is`/`is-deeply` to a stable winner
from a sound `(package, name, arg-type keys)` key
(`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md`).
A multi-aware compiled-call cache would key the *compiled* form on the same
tuple rather than on the name, so a multi could reach the light-call paths.

## Why it matters

It is the measured remainder of `todo/deep/vendor-real-test-module.md`. After
the multi-resolution keys were fixed, `roast/S03-buf/write-int.t` under the real
module still spends its per-assertion budget in this path, and the carrier's
env snapshot + writeback diff is the largest single item left. It is not
`Test`-specific: any module sub called in listop form pays it.

## Measurement protocol

`MUTSU_VM_STATS=1` does not currently count carrier entries for this opcode —
add one (`record_dispatch_entry_outcome("execcallpairs", ...)`) before
optimizing, so the fix has a deterministic counter rather than only wall-clock.
Then the 20 000-assertion loop `tmp/bench-ok-loop.raku` and `write-int.t` under
`MUTSU_REAL_TEST=1` are the two wall-clock checks.
