# Every full method dispatch rebuilds the whole lexical env

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
  mostly of that shape.
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

## Update (2026-09-06): the flatten also destroys the return merge

Re-measured after the multi-resolution cache started serving `Test`'s
assertions
(`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md`),
with the same unsound experiment (an env-var kill switch on
`flatten_scoped_env`): the 20 000-assertion `ok` loop goes **3.58 s -> 2.96 s**,
i.e. **17%** of what is left.

Callgrind says why it is worth more than its own `HashMap` clone. The single
largest self-cost item in the loop is `std::thread::local::LocalKey<T>::with`
(11.8% of the loop), and its dominant caller is
`call_compiled_function_named_inner` — **333 586 thread-local accesses across
600 named calls**, ~556 per call. Those come from the scoped-overlay *return
merge*:

```rust
for (k, v) in self.env().iter() { ... k.with_str(...) ... }
```

On a scoped env `iter()` is overlay-only, so that loop is O(callee writes) —
which is the whole point of Slice 6. But the method-dispatch flatten runs
*first* (`$output.say: $tap` inside `proclaim` is a full method dispatch), so by
the time the callee returns its env is FLAT and the merge iterates every name in
scope, calling `Symbol::with_str` two or three times per key.

So the flatten costs twice: once to build the merged map, and once more by
turning the frame's O(writes) return merge into an O(whole env) scan. Any fix
should be measured against the merge loop's `with_str` count, not just against
`env_deep_copies`.

Two cheaper sub-fixes, if the full "move the flatten to the consumers" change
stays too risky:

* `Env::flattened()` can return `parent.flattened()` directly when the overlay
  is empty and there are no tombstones — provably identical (`scoped_child`
  already derives an empty child's `file_sym` from the parent), and O(1) when
  the parent is flat.
* the merge loop's `k.with_str(is_routine_scoped_implicit_var)` runs for every
  key; the names it tests are a fixed handful, so interning them once and
  comparing `Symbol` ids removes a thread-local round trip per key.
