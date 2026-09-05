# `Supply.act` never serialized, and that is what corrupted the heap

`todo/deep/procasync-stress-segv.md` had tracked a rare, CI-only crash cluster since
2026-07-30: `roast/S17-procasync/stress.t` and `roast/integration/advent2014-day05.t`
occasionally killed the interpreter outright — SIGSEGV in some instances, and in the
best-documented one (CI run 33882510623, `gc-stress`) a glibc **corrupted-chunk abort
reached through `__libc_free`**, on a thread named `pool`, down a
`spawn_callable_promise` -> `call_supply_tap` stack. Six sessions and roughly 130 clean
local runs across four configurations had failed to reproduce it, and the recorded next
step was a memory checker.

This session reproduced it, root-caused it, and fixed it. The root cause is a missing
Raku guarantee, not a GC bug.

## `Supply.act` is `Supply.tap` plus one guarantee, and mutsu did not implement it

`raku-doc/doc/Type/Supply.rakudoc` on `act`: *"Differently from `tap`, the given code is
guaranteed to be executed by only one thread at a time."* mutsu delivers a tap
synchronously on whichever thread emitted into the supplier, and
`native_supplier_mut`'s emit loop took a serialize lock only for `supply {}` block
`whenever` taps (`supplier_serialize_group`, which is `None` for a plain `Supplier`). So
`.act` was a pure alias for `.tap`: N concurrent `start { $supplier.emit($_) }` blocks
ran the act callbacks on N pool workers at the same time.

That is exactly the shape `roast/integration/advent2014-day05.t` uses, with a comment
that says so — `# assume .act serializes` — and three act taps writing into one captured
`my @seen`:

```raku
$supply.act: { @seen[$_]   = "Fizz" if $_ %% 3 }
$supply.act: { @seen[$_]  ~= "Buzz" if $_ %% 5 }
$supply.act: { @seen[$_] //= $_ }
await do for 1..20 { start { sleep rand; $supplier.emit($_) } }
```

## Why unserialized act callbacks corrupt the heap

`@seen` is captured by the tap closures, which are `Gc`-shared across every worker that
invokes them, so the workers alias one `Gc<ArrayData>` node. The element-assign path
(`vm/vm_var_assign_index_named.rs`) sees `Gc::strong_count(items) > 1` and takes the
container-identity branch — the codebase's aliased-write primitive:

```rust
let use_inplace = crate::gc::Gc::strong_count(items) > 1;
let arr = if use_inplace { unsafe { gc_contents_mut(items) } } else { Gc::make_mut(items) };
Self::autoviv_resize(arr, i + 1, native_fill.clone())?;
```

`autoviv_resize` is a `Vec::resize`, i.e. potentially a **reallocation**. Two workers
doing that to the same `Vec` at the same time is a textbook double-free / use-after-free,
and `gc_contents_mut`'s own `# Safety` clause already forbids it: it requires that
*"concurrent structural mutation from another thread remains routed through the
synchronized shared-store lanes"*. Nothing routed the act-tap store anywhere; the
`shared_vars` lane is seeded from a `start` block's own captured env, and `@seen` is
captured by the tap, not by the `start` block.

The decisive evidence is a Rust bounds check firing in the middle of that block:

```
panicked at core/src/panicking.rs: index out of bounds: the len is 3 but the index is 19
  6: exec_index_assign_expr_named_op_inner::{{closure}}   (vm_var_assign_index_named.rs:2338)
  7: Value::with_array_mut
 14: exec_one_dispatch
```

`arr[19]` on a length-3 `Vec` *immediately after* `autoviv_resize(arr, 20)` returned Ok is
only possible if another thread structurally replaced that `Vec` in between. The same
race, landing on the allocator instead of on the bounds check, is the CI SIGABRT.

## The fix

`Supply.act` now registers the supplier in the existing supply-block serialize-group
machinery (`set_supplier_serialize_group_if_absent`, keyed on the supplier's own id), so
every emit into a supplier that has an act tap holds the group lock across its whole
tap-dispatch loop. The lock is re-entrant per thread, so a callback that synchronously
re-emits into its own supplier does not self-deadlock, and a `supply {}` block's own
(broader) grouping still wins where both apply.

mutsu serializes the *dispatch*, not the individual callback — it runs all of a
supplier's taps in one loop, so the dispatch is the unit it can lock. That is strictly
stronger than both the documented guarantee and rakudo 2026.06, which serializes each act
callback only against itself (`t/supply-act-serializes.t` reports max=2 under rakudo for
sibling taps). Stronger mutual exclusion can only reduce concurrency, never change a
correct program's result — and it is what makes a container shared by several act bodies
safe against concurrent emitters, which is the entire reason `.act` exists.

Pinned by `t/supply-act-serializes.t`: an act callback never overlaps itself, sibling act
taps never overlap, and every concurrent act write to a shared array lands.

## Measurements

All on a `--profile profiling` build under the `gc-stress` job's environment
(`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`).

| Workload | Before | After |
| --- | --- | --- |
| `roast/integration/advent2014-day05.t`, native, 8-way concurrent | 1 failure / 64 runs | 0 / 80 |
| same file, ASan build, 6-way concurrent | 2 failures / 36 runs | — |
| `tmp/probe-d.raku` (the file's three blocks, looped), 6-way concurrent | 4 bad rounds / 600, plus one bounds-check panic | 0 / 900 |
| `tmp/probe-e.raku` (bisect harness), 6-way concurrent | 1 bad round / 600 | 0 / 900 |

The pre-fix failures were not ordering noise. Each emitted `n` touches only index `n`, so
the expected array is fully determined however the emits interleave; a failure printed a
literally lost write (`... 16 17 18 19 Buzz` where index 18 must be `Fizz`), and in the
worst instance most of the array (` 2  4  Fizz     11    FizzBuzz     Buzz`).

## What the memory checkers did and did not show

Recorded because the ticket's standing advice was "reach for valgrind/ASan", and the
result is a useful correction:

- **`valgrind --tool=memcheck`**: 0 errors, on both a minimal supply-race script and the
  looped blocks. Memcheck serializes threads onto one core, which suppresses the very
  interleaving the bug needs. It cannot find this class.
- **`valgrind --tool=helgrind`**: 6656 errors from 1000 contexts, and unusable — it could
  not symbolize the binary (`??? (in .../mutsu)`) or unwind past the first frame in an
  optimized build, so every report was noise-shaped.
- **AddressSanitizer** (`-Zsanitizer=address`, nightly): no ASan report, but its ~10x
  slowdown widened the race window enough to make the *assertion* failure surface at
  2/36 — which is what pointed at a lost update rather than at a GC bug.

The tool that actually located it was mutsu's own `Vec` bounds check, hit by running the
real workload under a concurrent load harness. For a race whose damage is a `Vec::resize`
on an aliased node, the interpreter's existing safety checks are a better detector than a
checker that serializes threads away.

## What is still open

The general hazard is unchanged: `gc_contents_mut` has 149 call sites and none of them is
synchronized against another VM thread. `.act` was the reachable path in the crashing
workload, so closing it removes this crash cluster, but any other route that shares one
container between two VM threads has the same exposure. That remains ADR-0001 layer 3c
and is recorded in `todo/deep/procasync-stress-segv.md`.
