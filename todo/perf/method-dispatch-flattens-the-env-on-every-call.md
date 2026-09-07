# Every full method dispatch rebuilds the whole lexical env

> **Read "Update (2026-09-06b)" at the bottom before starting.** The obvious
> fix -- removing the guard and moving it to the consumers that actually
> iterate -- was implemented in full on 2026-09-06, validated clean over both
> suites, and **measured neutral-to-negative**. Do not redo it without a
> different plan.

`exec_call_method_mut_op_impl` calls `flatten_scoped_env()` before dispatching
any method that misses the pure-read accessor fast path. On a scoped env — which
is what every enclosing call frame installs — `Env::flattened()` walks the parent
chain and rebuilds the map, so the cost of *one method call* is linear in how
many names are in scope anywhere up the stack. Two such calls per assertion is
enough to make the whole program's speed a function of its env size.

## The measurement

2000 `ok 1, "x"` assertions under the vendored upstream `Test`
(`MUTSU_REAL_TEST=1`, release, one run), with N unused `our` variables added to
the mainline purely to grow the env, and the same file with the
`flatten_scoped_env()` call commented out (an unsound experiment, purely to size
the prize):

| padding vars | as shipped | flatten removed |
| --- | --- | --- |
| 0 | 0.543 s | 0.487 s |
| 300 | 1.213 s | — |
| 600 | 1.841 s | — |
| 900 | 2.363 s | **0.958 s** |

Slope as shipped: **~0.98 ns per env entry per assertion**. With the flatten
gone the slope essentially disappears — a program that declares 900 file-scope
variables stops paying for them on every method call. `roast/S03-buf/write-int.t`
under the real `Test` goes 45.4 s -> 42.2 s from the same experiment (its own env
is small, so it sees the constant, not the slope).

`MUTSU_VM_STATS` corroborates: 3 `env_deep_copies` and 2 `clone_env`s per
assertion, and the padded workload's stack profile spends 12% of samples in
`HashMap::clone<Symbol, Value>` under `Env::insert_sym` / `Env::flattened`.

## The site

```rust
// Beyond the pure-read accessor fast path above, full method dispatch may
// capture/iterate the env; collapse a transient scoped overlay env to a
// flat env so the full lexical view is seen.
self.flatten_scoped_env();
```

(`src/vm/vm_call_method_mut_ops.rs`, reached from `exec_call_method_mut_op`.)

It is the *universal* form of the scoped-env safety invariant
(`docs/vm-dual-store.md` Slice 6: anything that captures or iterates the env for
a full lexical view flattens it first). The guard is correct and was already
moved once for cost — it sits after the accessor read so a `$.attr` read inside
a scoped method body does not collapse the overlay. What it still does not do is
distinguish a dispatch that will actually capture or iterate the env from one
that will not. `$output.say: $tap` and `$desc.Str` — the two method calls a
`Test` assertion makes — are native methods that never look at a lexical by
name, and they pay a full env rebuild each.

## What was ruled out

Two plausible-looking suspects on the *sub* call path are NOT the cost, measured
individually:

- `call_compiled_function_named_inner`'s `Value::make_sub(..., self.clone_env())`
  — the flat env carried by the `Sub` pushed for `callframe().code`. Replacing it
  with an empty env changed the 2000-assertion time by nothing (1.630 s -> 1.625 s
  for three runs). It IS an `Env::flattened()` per named call, but for a caller
  whose env is flat that is an `Arc` bump, and the workload's named calls are
  mostly of that shape. **(This finding is the key to the failed attempt below:
  that `clone_env` is only cheap *because* the method-dispatch guard already
  flattened it. Remove the guard and it becomes a real per-call flatten.)**
- `push_caller_env()` — this is a plain `Env` clone (`Arc` bumps), not a flatten.

So the fix belongs on the method path, not the sub path.

## Shape of a fix

Reads do not need a flat env: a scoped env's `get` walks its parent chain. Only
*full-view iteration* does. So the direction is to move the flatten from "every
full method dispatch" to the consumers that actually iterate — closure capture,
`clone_for_thread`, `callframe`/`CALLER::`, pseudo-stash and `.WHO`-style
enumeration — or to gate it on a property of the resolved callee (a native method
with no env-capturing body cannot need it).

The risk is precisely what `docs/vm-dual-store.md` warns about: a single missed
iteration site reads one tier and silently reports a truncated lexical view, and
the failure is a wrong answer rather than a crash. So this wants the full roast
suite as its check, and probably an audit of `flatten_scoped_env`'s callers
recorded alongside the change.

## Why it matters

It is the measured remainder of `todo/deep/vendor-real-test-module.md`'s last
timeout: after the 2026-09-06 dispatch-cache fix
(`news/2026-09/imported-module-sub-reaches-the-cached-dispatch.md`) removed the
three per-call registry walks, `write-int.t` still runs 45 s against a 30 s
budget. This experiment accounts for ~3 s of that gap and, more importantly, for
the entire dependence of method-call cost on program size — which every
env-heavy Raku program pays, not just `Test`.

## Measurement protocol

Iterate on `MUTSU_VM_STATS`'s `clone_env` / `env_deep_copies` counters (they are
deterministic and optimization-independent, so the debug build is enough), then
confirm on release with the padding table above: the cleanest success signal is
that the per-assertion cost stops scaling with env size at all.

## Update (2026-09-06a): the flatten also destroys the return merge

The scoped-overlay *return merge* in `call_compiled_function_named_inner` is
O(callee writes) on a scoped env and O(whole scope) on a flat one:

```rust
for (k, v) in self.env().iter() { ... k.with_str(...) ... }
```

A full method dispatch in the callee body flattens the env, so by the time the
callee returns the merge walks every name in scope. Callgrind put
`std::thread::local::LocalKey<T>::with` at 11.8% of the assertion loop with this
merge as its dominant caller (~556 thread-local accesses per named call).

**That half is fixed** --
`news/2026-09/scoped-overlay-return-merge-stops-paying-for-a-flattened-env.md`
made the merge's per-key work a single memoized `Symbol::flags()` byte, skipped
keys the callee never rebound (`Value::same_binding`), and short-circuited
`Env::flattened()` for an empty overlay: 658 k -> 535 k instructions per
assertion. The compounding is gone; the flatten's own cost is not.

## Update (2026-09-06b): the relocation fix was implemented, measured, REVERTED

### What was built

The full "move the flatten to the consumers that iterate" change, end to end:

1. `Env::lexical_view()` -- the whole visible lexical view as a flat env
   (`self.flattened()`; an `Arc` bump when already flat).
2. `#[track_caller] debug_assert!(!self.is_scoped())` on `Env::iter` / `keys` /
   `values` / `values_mut` / `len`, turning "a consumer sees a truncated scope"
   from a silent wrong answer into a debug panic that names the *caller*.
3. The three return merges (`vm_call_named_inner`, `vm_call_fast`,
   `vm_closure_dispatch`) switched to `overlay_iter()` -- semantically
   identical, since `iter()` on a scoped env already IS the overlay.
4. All four `flatten_scoped_env()` call sites deleted, and the helper with them.
5. Every site the assertion caught migrated to `lexical_view()`.

### The site inventory (the reusable part)

Renaming the accessors to force compile errors found **133** call sites -- far
too many to hand-classify. The `#[track_caller]` assertion plus a sweep script
narrowed that to the sites *actually* reached with a scoped env: **44**,
converging over five rounds (35, 13, 9, 9, 1) across `t/` (3723 files) and the
roast whitelist (1436 files) on the debug binary.

Nearly all wanted the full view: pseudo-stashes (`MY::`/`OUTER::`/`LEXICAL::`),
identity searches (`find_var_by_identity`, `find_instance_in_env`), the
`__mutsu_sigilless_alias::` scan, operator-name collection, class-body and
module-exit lexical snapshots, the END-phaser capture overlay. Only three wanted
overlay-only (the return merges), plus the GC's `SubData`/`LazyList` traces,
where overlay-only is positively *required*: a scoped env's parent tier is an
`Arc<Env>` this node does not own, so tracing a flattened view would be exactly
the edge over-claim `gc_overlay_uniquely_owned` exists to prevent.

If this is retried, rebuild the sweep: run each test file, `awk` the
`panicked at src/...` line together with the message line that follows it, and
keep only the ones mentioning a scoped env -- a test that panics for an
unrelated reason (`t/hyper-race-panic-boundary.t` deliberately overflows an
index) must not show up as a target. Note that a snippet run in a SUBPROCESS
(`is_run` / `run-snippet`) hides its panic from the sweep: two sites
(`run.rs`'s END-phaser overlay, `accessors_stash.rs`'s
`package_namespace_exists`) surfaced only as ordinary `make test` failures.

### Correctness

Clean. `make test` and the roast whitelist both passed with only the known
environmental failures, and the debug-assert sweep was silent over both suites.

### Performance: it does not pay

Deterministic instruction counts (callgrind, 300 `ok 1, "x"` under
`MUTSU_REAL_TEST=1`, one-assertion baseline subtracted) plus wall clock on an
idle box. `pad-N` is a 2000-iteration `ok` loop with N unused `our` variables:

| build | loop Ir | pad-0 | pad-900 | 20 k `ok` loop |
| --- | --- | --- | --- | --- |
| as committed (flatten present) | **158.8 M** | 0.326 s | 1.207 s | 2.945 s |
| flatten removed, sites migrated | 163.8 M | 0.355 s | 1.047 s | 3.164 s |
| unsound kill-switch (flatten never runs) | 144.4 M | -- | -- | -- |

The size-scaling improves only from 3.7x to 2.9x, the base case gets ~9%
*worse*, and the instruction count is 3% worse overall -- against a kill-switch
that promised 10%.

### Why it does not pay

**The flatten relocates rather than disappears.** Every removal exposed another
consumer that genuinely needs a full view *per call*:

1. `Value::make_sub(..., self.clone_env())` for `callframe().code` -- cheap only
   because the guard had already flattened (see "What was ruled out" above).
   Capturing the scoped env unflattened is sound (a scoped env is already an
   immutable snapshot: later caller writes un-share it via `cow_mut`) and
   recovered part of it.
2. `exec_block_local_scope_op`'s `env_had_before` key-set snapshot -- O(env) on
   every block-scope opcode, twice per assertion, and now a flatten too.
   Replacing it with an O(1) `Env` snapshot + `contains_key_sym` recovered a
   little more.
3. The `__mutsu_sigilless_alias::` reverse-alias scan on every named `SetLocal`.
   Gating it on a new monotonic `sigilless_alias_possible()` latch recovered a
   little more.

Even with all three the total stayed above the committed baseline. And **tried
on their own against the committed tree, (2) and (3) also measured neutral**
(259.3 M vs 258.2 M): with the guard in place the env at those sites is already
flat, so the O(env) walk they remove is the cheap kind. They are not
independently shippable wins -- do not resurrect them in isolation.

### What a real fix would have to be

Not a relocation. Either:

* **a representation change** that makes a full lexical view structurally free
  -- a persistent/HAMT overlay map, where `flattened()` is O(overlay) with
  sharing instead of O(scope) with a copy. That costs a slower `get`, which is
  far hotter, so it needs its own measurement before anything else; or
* **removing the per-call need for a full view** -- the three consumers above
  are each avoidable in principle (a lazy `callframe().code` env, a block-scope
  declaration list that never consults the env, an alias table that is not
  env-resident). That is three separate, individually-measurable tickets, and
  only after all of them is deleting the guard worth re-testing.

Either way, re-run the `MUTSU_NO_FLATTEN` kill-switch experiment first: the
prize was 17% when this ticket was filed, 10% after the return-merge fix, and it
keeps shrinking as the per-call full-view consumers go away.

## Update (2026-09-07): the chain flatten clones the root once; the guard is unchanged

`Env::flattened` on a multi-tier chain used to recurse through
`parent.flattened()` and then clone *that* result to layer the leaf overlay
on top, so the method-dispatch guard in a routine two frames deep
(`ok` -> `proclaim` -> `$output.say`) materialized the whole scope once per
tier. #7465 walks to the flat root, clones its map once and layers every
tier root-ward first (the single pass `filtered_flat` already used). On the
real-`Test` assertion loop that took `Env::flattened` from 24.6k to 15.9k Ir
per assertion and the guard's own row (`flatten_scoped_env`) from 18.1k to
8.8k. That is the cost of being scoped two frames deep, not the cost of the
guard: one whole-scope clone per method dispatch past the accessor fast path
remains, and everything in "Update (2026-09-06b)" above still applies to
removing it.
