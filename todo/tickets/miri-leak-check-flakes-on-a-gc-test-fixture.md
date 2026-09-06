# `cycle_with_external_ref_survives` leaves its cycle for Miri's leak check to find

Observed 2026-09-06 on PR #7394 (CI run 34033980401, job 101488591625). The
`miri` job's step 1 ("GC / container primitives") failed with four
`error: memory leaked` reports, every one of them backtraced to
`gc::collect::tests::cycle_with_external_ref_survives` — two `Gc<TestNode>`
nodes (64 bytes each) plus the `Vec` inside each node's
`Mutex<Vec<Gc<TestNode>>>` (32 bytes each).

Every test in the step reported `ok`; the summary line was
`test result: ok. 56 passed; 0 failed; 1 ignored; 873 filtered out`. The job
failed only on Miri's process-exit leak check, which step 1 deliberately leaves
enabled.

**Confirmed non-deterministic**: re-running the same job on the same commit
(job 101491298645) passed step 1. So this is a flake, not a regression — but it
is a flake on a *required* check, on a gate that only runs when a PR touches
`src/value/` or `src/gc/`, so it will keep blocking such PRs at random until
fixed. Note that `flaky-tests.txt` cannot help here: that mechanism retries
`t/`/roast files, not a `cargo miri test` step.

## Why the test leaks

`src/gc/collect.rs`:

```rust
fn cycle_with_external_ref_survives() {
    ...
    let a = TestNode::new();
    let b = TestNode::new();
    TestNode::link(&a, &b);
    TestNode::link(&b, &a);
    a.buffer_as_candidate();
    b.buffer_as_candidate();
    drop(b);
    let _ = collect_cycles();
    assert_eq!(DROPS.load(Ordering::Relaxed) - before, 0, "live external ref => not garbage");
    assert_eq!(a.children.lock().unwrap().len(), 1);
    drop(a);          // <-- last statement
}
```

The `a` <-> `b` cycle becomes unreachable at that final `drop(a)`, and the test
does not collect afterwards. Its own assertions are all made before the drop, so
the test is correct as an assertion; it simply leaves its fixture behind. Whether
those four allocations are still live at process exit therefore depends on
whether some *later* test's `collect_cycles()` happens to sweep them — which is
not a property this test controls.

## Why that is timing-dependent

`drop(a)` reaches `buffer_candidate` (`src/gc/gc_ptr.rs:848`), which explicitly
**never collects inline** — "this runs from `Gc::drop`, which can hold a borrow
(design §1.2)". All it does is `note_candidate_push` / `note_buffer_len`, arming
a *pending* collect for a later safepoint. So nothing in this test ever reclaims
its own fixture; the sweep is left to whichever later safepoint fires, which
under `--test-threads=1` means during some subsequent test.

Whether that later collection actually runs is not deterministic: it needs the
ADR-0003 size threshold to have been crossed, and the cycle scan declines
outright while a mutator worker is active (see the sibling test
`collect_is_deferred_while_a_mutator_worker_is_active`, and the stop-the-world
attempt in `gc::stw`, whose own tests spawn workers). On a slower or
more-contended runner the sweep can simply not happen before exit.

## Not reproducible locally

On this 12-core box, with the pinned CI toolchain and the exact CI command
(`nightly-2026-08-01`, `MIRIFLAGS=-Zmiri-disable-isolation`,
`cargo miri test --no-default-features --features native --lib gc:: --
--test-threads=1 --skip gc::soundness_smoke`), the step exits 0 with zero
`memory leaked` reports — measured four ways: the full `gc::` filter twice, the
`gc::collect::tests` group alone, and `cycle_with_external_ref_survives` alone.
So the leak needs the runner's timing, not a code difference.

## The obvious fix does NOT work — measured, do not re-propose it

The tempting one-liner is to reclaim the fixture in the test that creates it, the
way `self_cycle_is_reclaimed` and `two_node_cycle_is_reclaimed` do:

```rust
    drop(a);
    let _ = collect_cycles();
```

That reclaims nothing. Measured 2026-09-06 by temporarily adding the call plus
`assert_eq!(DROPS.load(Ordering::Relaxed) - before, 2)`:

```
assertion `left == right` failed: PROBE: the fixture cycle is reclaimed by an explicit collect
  left: 0
 right: 2
```

The reason is that the test's *earlier* `collect_cycles()` already drained `a`
and `b` out of the candidate buffer, so by the time the second one runs the
collector has no suspects to scan. Whatever the real fix is, it has to get the
nodes buffered again after the final drop (or arrange for the drop itself to
buffer them), which needs someone who knows the Bacon-Rajan bookkeeping —
`#[cfg_attr(miri, ignore)]` is the other option, but the CI comment block is
explicit that ignoring is only legitimate when the property is not about
provenance *and* something else still runs the test natively.

Not attempted as a drive-by in #7394, whose diff touches no GC code.
