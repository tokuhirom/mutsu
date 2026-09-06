# `cycle_with_external_ref_survives` no longer leaves its cycle for Miri

The `miri` CI job failed intermittently on a *required* check with four
`error: memory leaked` reports, every one backtraced to
`gc::collect::tests::cycle_with_external_ref_survives` — the two `Gc<TestNode>`
nodes plus the `Vec` inside each node's `Mutex<Vec<Gc<TestNode>>>`. Every test
in the step reported `ok`; only Miri's process-exit leak check, which step 1
deliberately leaves enabled, failed. Re-running the same job on the same commit
passed. Because the job runs only when a PR touches `src/value/` or `src/gc/`,
it blocked exactly those PRs, at random.

## Why it leaked

The test's assertions are all made while `a` is alive; its last statement was
`drop(a)`, which is the moment the `a` ↔ `b` cycle becomes unreachable. Nothing
collected afterwards, so whether those four allocations were still live at
process exit depended on whether some *later* test's `collect_cycles()` happened
to sweep them — not a property this test controls, and the source of the
non-determinism.

## Why the obvious fix does not work

Appending `let _ = collect_cycles();` after the drop reclaims nothing, and the
ticket recorded that measurement so it would not be re-proposed. The reason is
now written into the test: the earlier `collect_cycles()` already **drained**
both nodes out of the candidate buffer, and `Gc::drop` does not put them back —
unit tests run with the GC off (`gc_enabled` is `!cfg!(test)`), so the
`buffer_candidate` call in `Drop` never runs. The collector reaches the second
call with no suspects to scan.

## The fix

Re-buffer **before** releasing the last handle. A candidate entry holds only a
`Weak`, so buffering while the handles are still live is fine: the entries stay
upgradable across the drops, because the cycle keeps both nodes alive until the
collector breaks it.

```rust
let b = a.children.lock().unwrap()[0].clone();
a.buffer_as_candidate();
b.buffer_as_candidate();
drop(b);
drop(a);
let stats = collect_cycles();
assert_eq!(stats.reclaimed_nodes, 2);
assert_eq!(DROPS.load(Ordering::Relaxed) - before, 2);
```

The two new assertions are the point: the test now *proves* it reclaims its own
fixture rather than hoping a later test does, so it can no longer be a
leak-check candidate at exit. The property the test existed to check — a cycle
with a live external reference is not garbage — is unchanged and still asserted
before the drops.

Verified green under the exact CI invocation
(`nightly-2026-08-01`, `MIRIFLAGS=-Zmiri-disable-isolation`,
`cargo miri test --no-default-features --features native --lib gc:: --
--test-threads=1 --skip gc::soundness_smoke`): 56 passed, zero
`memory leaked` reports. Also green under `cargo test --lib gc::` both with the
GC off (the default in tests) and with `MUTSU_GC=on`.
