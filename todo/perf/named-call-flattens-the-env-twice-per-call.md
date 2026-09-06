# The named call path flattens the whole env twice on every call

`call_compiled_function_named_inner` — the general (non-light) call path, which
every routine with a heavy signature takes — rebuilds a flat copy of the entire
lexical env **twice per call**, before it has run a single opcode of the body.
The cost is linear in how many names are in scope, so it grows with the program
rather than with the call.

## The measurement

2000 `ok 1, "x"` assertions under the vendored upstream `Test`
(`MUTSU_REAL_TEST=1`, release), with N unused `our` variables added to the
mainline purely to grow the env:

| padding vars | wall clock |
| --- | --- |
| 0 | 0.609 s |
| 300 | 1.213 s |
| 600 | 1.841 s |
| 900 | 2.363 s |

Slope: **~0.98 ns per env entry per assertion**, i.e. roughly a full copy of the
env two to three times per call. `MUTSU_VM_STATS` agrees: 2 `clone_env` and 3
`env_deep_copies` per assertion.

Nothing in `ok 1, "x"` reads those variables. The whole slope is bookkeeping.

## Where the two flattens are

```rust
loan_env!(self, push_caller_env());                  // (1)
let sub_val = Value::make_sub(
    ..., self.clone_env(),                           // (2)
);
...
{ let parent = self.env().clone();                   // the cheap one
  self.set_env(crate::env::Env::scoped_child(parent)); }
```

`clone_env()` is `Env::flattened()`. On a flat env that is an `Arc` bump, but on
the hot path the live env is a **scoped overlay** (every enclosing frame
installed one), and flattening a scoped env walks the parent chain and rebuilds
the map. So:

1. `push_caller_env()` snapshots the caller's env so a callee's `CALLER::<$x>`
   can read it. Almost no call is ever the target of a `CALLER::` lookup.
2. The `Sub` value pushed on the block stack carries a flat env so
   `callframe().code` can expose a full lexical view. Almost no call has its
   frame introspected.

Both are eager snapshots taken for a consumer that usually never runs. Note
the third `clone_env` at the same site (`make_sub`) is the *reason* the env's
`Arc` is then shared, which is what turns the callee's first by-name write into
an `Arc::make_mut` deep copy — so (2) costs twice.

## What a fix has to preserve

The scoped-env safety invariant (`docs/vm-dual-store.md` Slice 6) is that
anything capturing or cloning the env across a boundary flattens it first, so no
full-view *iteration* consumer is starved of parent lexicals. Reads are fine
unflattened: a scoped env's `get` walks its parent chain. So the shape of a fix
is to store the scoped env (an `Arc` bump) and flatten at the point of use —
`get_caller_var` / `callframe` / `.WHO`-style iteration — rather than at every
call. The consumers are the risk: a single missed iteration site reads one tier
and silently reports a truncated lexical view.

An intermediate step that needs no consumer audit: make the `Sub` value's env
lazy (built on first introspection from a retained scoped env), which removes
one flatten and, with it, the sharing that forces the callee's first write to
deep-copy.

## Why it matters

This is the measured remainder of `todo/deep/vendor-real-test-module.md`'s last
timeout: after the 2026-09-06 dispatch-cache fix removed the three per-call
registry walks, `roast/S03-buf/write-int.t` still runs 45 s against a 30 s
budget, and the resolutions were only ~6% of it. Every heavy-signature call in
any program pays this, not just `Test` — but `Test` is where it is measurable,
because an assertion is nothing *but* a call.

## Measurement protocol

Iterate on `MUTSU_VM_STATS`'s `clone_env` / `env_deep_copies` counters (they are
deterministic and optimization-independent, so the debug build is enough), then
confirm wall clock on release with the padding table above — it is the cleanest
signal that the per-call cost stopped scaling with env size.
