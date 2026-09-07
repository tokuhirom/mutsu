# A listop call (`ExecCallPairs`) reaches none of the compiled-call caches

`ok 1, "x"` and `is $got, $expected, "..."` — the shape every roast assertion is
written in — compile to `OpCode::ExecCallPairs`, not to `CallFunc`, because the
parser injects a `__mutsu_test_callsite_line` **named** argument into every test
call and a statement-level listop call with a named argument is exactly what
that opcode is for. It has only three arms (`src/vm/vm_call_exec_ops.rs`,
`exec_exec_call_pairs_op`):

1. `find_compiled_function(compiled_fns, name, args)` — the *caller's* compiled
   table, which an imported module sub is never in;
2. `try_native_function` — a Rust builtin;
3. the **carrier fallback**: `snapshot_carrier_overwritable_env(code)`,
   `begin_carrier`, `exec_call_pairs_values`, `end_carrier`,
   `writeback_carrier_writes`, `carrier_writeback_changed_aggregates`.

`CallFunc`'s three name-keyed fast caches — `pos_light_cache`,
`light_call_cache`, `otf_call_cache` — are all in `exec_call_func_op` and none
of them is consulted here. That is still true, and the counter this ticket asked
for now proves how total it is.

## Measured, 2026-09-06 — the structural claim holds, the cost claim does not

The counter the ticket asked for is in place:
`record_dispatch_entry_outcome("execcallpairs", …)` reports
`compiled` / `native` / `carrier` under `MUTSU_VM_STATS=1`. On a 200-assertion
`ok 1, "x"` loop it reports `execcallpairs:carrier=200` — 200 of 200 — under
`MUTSU_REAL_TEST=1` *and* under the native `Test` handlers. So yes: every
assertion in every roast file takes the carrier arm.

But the carrier is not what that costs. Profiling the release build with
callgrind on a 2 000-assertion loop (`tmp/bench-ok-2k.raku`, ~608 KIr per
assertion) gives these inclusive shares of the whole run:

| | share |
| --- | --- |
| `exec_exec_call_pairs_op` | 92.61% |
| ⤷ `exec_call` (the carrier's callee) | 91.39% |
| ⤷⤷ `call_routine_def` | 90.99% |

**The entire carrier arm — arm 1's miss, arm 2's miss, the env snapshot, the
write log, the writeback diff — is the 1.2% between the first two rows.** Adding
30 unrelated locals to the caller frame (which is what the snapshot and the
aggregate writeback are O(n) in) moved a 20 000-assertion run from 3.47 s to
3.68 s best-of-3: ~6%, and only for a frame that wide.

Nor would a cache hit buy back much of the rest: `multi sub ok(Mu $cond, $desc =
'')` has a defaulted parameter, so it is ineligible for both
`is_light_call_eligible` and `is_positional_light_call_eligible`. A cache hit
would land on `call_compiled_function_named` — exactly where `call_routine_def`
already lands. Gap (b) below (a multi-aware compiled-call cache) therefore buys
`ok`/`is` nothing at all on top of the 1.2%.

### What the per-assertion budget actually goes on

Same profile, same run, inclusive shares (before the fix landed below):

| | share |
| --- | --- |
| `bind_function_args_values` | 13.16% |
| `push_multi_dispatch_frame` → `resolve_all_multi_candidates` | 8.53% |
| `Env::insert_sym` | 6.32% |
| `type_matches_value` | 5.72% |
| `eval_param_default` | 5.69% |
| `alloc::fmt::format::format_inner` | 94 712 calls — ~47 `format!` per assertion |

The 8.53% row is fixed: `resolve_all_multi_candidates` walked every registry key
once per enclosing package on every dispatch, and now filters the
`fn_keys_for_base` index instead
(`news/2026-09/multi-candidate-gather-uses-the-base-name-index.md`, -7.5% wall
clock on the assertion loop). The others are open.

## What is left

**(a) `ExecCallPairs` has no OTF cache at all**, and arm 1 does not reach the
light paths either — it goes straight to `call_compiled_function_named`. Worth
~1.2% on the Test path, more for an imported *light-eligible* sub reached in
this shape, which is rare in practice: the compiler routes most named-argument
listop calls to `CallFuncNamed` (which does have the caches), so `ExecCallPairs`
is in practice the test-assertion shape plus a tail statement call with named
args. Wiring the cache in is still the architecturally right move — it retires a
carrier fallback — but it is a ~1% change that has to re-derive `exec_call`'s
pre-resolution guards (native test handlers, `make`/`made`, the wrap chain, the
`JSON::Fast`/`JSON::Tiny` native override, `EVAL`) at the new call site, and
must reproduce `sanitize_call_args_owned` so the injected callsite-line pair
does not reach the binder as a stray named argument. **Do not do this expecting
a speedup**; do it as a slow-path retirement, with the counter above as the
before/after evidence.

**(b) `otf_call_cache` skips multi names by construction.** Its guard is
`!self.has_multi_candidates_cached_sym(name_sym)`, because the entry is keyed on
the name alone and a multi's winner depends on the argument types.
`func_multi_resolve_cache` now resolves a type+arity-deterministic multi from a
sound `(package, name, arg-type keys)` key
(`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md`),
so a multi-aware compiled-call cache could key the *compiled* form on the same
tuple. As noted above this is worth nothing for `ok`/`is` specifically; it would
pay only for a multi whose winner *is* light-eligible.

**(c) The rows in the budget table above.** `bind_function_args_values` at 13%
and the ~47 `format!`s per assertion are each larger than (a) and (b) combined.
They want their own `todo/perf/` entries once someone has attributed them; the
`format!` traffic is diffuse (`var_type_constraint` alone is called 15× per
assertion) and looks like the more tractable of the two.

## Measurement protocol

```
cargo build --release
# structural: which arm does each ExecCallPairs take?
MUTSU_REAL_TEST=1 MUTSU_VM_STATS=1 ./target/release/mutsu tmp/bench-ok-small.raku 2>&1 >/dev/null \
  | grep dispatch-entry
# cost: deterministic, load-independent
MUTSU_REAL_TEST=1 valgrind --tool=callgrind --callgrind-out-file=cg.out \
  ./target/release/mutsu tmp/bench-ok-2k.raku
callgrind_annotate --inclusive=yes cg.out
```

`tmp/bench-ok-2k.raku` is `use Test; plan 2000; for ^2000 { ok 1, "x" }`; the
20 000-assertion version is the wall-clock check (release, interleaved against a
baseline binary built in a `git worktree` — a single-binary before/after on this
box is ±15% noise). `roast/S03-buf/write-int.t` under `MUTSU_REAL_TEST=1` is the
realistic second check. Reference: rakudo runs the 20 000-assertion loop in
~0.50 s on this box against mutsu's 3.36 s.
