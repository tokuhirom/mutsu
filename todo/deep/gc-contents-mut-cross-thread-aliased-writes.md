# `gc_contents_mut` aliased writes are unsynchronized across VM threads

This is the residue of `todo/deep/procasync-stress-segv.md`, which was closed on
2026-09-05 by finding and fixing the path that actually crashed
(`news/2026-09/supply-act-serialization-and-the-concurrency-crash-cluster.md`). Read that
entry first: it has the evidence, the measurements, and the negative results from the
memory checkers, none of which is repeated here.

## The general hazard

`crate::gc::gc_contents_mut` is, in its own words, *"the codebase's single
aliased-container-write primitive"*: given a `Gc<T>` whose strong count is greater than
one, it hands out a `&mut T` so that a write through one alias is visible through every
holder of the same node. That is how mutsu implements Raku container identity, and it is
correct on one thread.

Its `# Safety` clause already names the gap:

> The caller must ensure that ... concurrent structural mutation from another thread
> remains routed through the synchronized shared-store lanes (the narrow cross-thread
> race deferred to ADR-0001 layer 3c).

There are **149 call sites** and none of them establishes that. The shared-store lanes
(`runtime/runtime_shared_vars.rs` — `assign_array_elem_to_shared_var`,
`shared_array_elem_set`, `push_to_existing_shared_var`, `__mutsu_atomic_arr::`) are seeded
from a `start` block's own captured environment. Any *other* way a container comes to be
aliased by two VM threads bypasses them entirely, and then two threads can call
`Vec::resize` / `HashMap::insert` on the same allocation at once. The damage is a
double free or a use-after-free, which surfaces as a glibc corrupted-chunk abort inside
`__libc_free` or as a bare SIGSEGV — both observed on CI, both with `Failed: 0`.

The demonstrated route was a `Supply.act` tap closure's captured `my @seen`, aliased by
every pool worker that emitted into the supplier. That one is closed: `.act` now takes the
supply serialize group, so its callbacks cannot run concurrently. Other routes are not
enumerated. Candidates worth auditing, roughly in order of how easily user code reaches
them:

- a `.tap` (not `.act`) callback's captured containers — `.tap` has no serialization
  guarantee in Raku either, so this cannot be fixed by locking the dispatch; it needs the
  store to be synchronized, or the capture to be routed through a shared lane
- a `Channel` / `Promise` combinator callback's captured containers
- an object attribute (`Instance` `AttrMap`) reachable from two threads, where the
  element-store path takes the same `strong_count > 1` in-place branch
- `Thread.start` bodies, which do not go through `clone_for_thread_for_block`'s seeding

## Why it is deep, not a ticket

Every candidate remedy is an architectural call:

1. **Synchronize the primitive.** `gc_contents_mut` returns a `&mut T` that outlives the
   call, so a guard cannot live inside it; the lock would have to be taken by each of the
   149 callers around its own mutation region. Mechanical, but a large diff and a
   deadlock-analysis obligation at every site that can call user code while holding it.
2. **Widen the shared-store lanes** so any container that becomes cross-thread reachable
   is registered, not just a `start` block's captures. This is the "right" fix in the
   sense that it reuses the existing serialization, but "becomes cross-thread reachable"
   is a reachability question mutsu has no answer to today.
3. **Make the lock free when it is free.** Both of the above can gate on a
   process-global "more than one VM mutator thread is live" flag, so single-threaded
   programs pay one relaxed atomic load. That is what makes either affordable, and it
   should be measured against the bench CI before being assumed cheap.

This is ADR-0001 layer 3c territory and should get a `Proposed` ADR before any code.

## The one instance that is NOT explained by the fixed path

`roast/S17-procasync/stress.t` was the original ticket's subject: it died of SIGSEGV once,
on CI run 30590633128 (2026-07-30), in the rakudo#3299 block that starts 1200
`Proc::Async` instances inside a `react`. It has never reproduced since — roughly 130
targeted local runs across four configurations, plus this session's runs, are all clean —
and it has no `Supply.act` tap, so the `.act` fix does not obviously cover it. It uses
`react`/`whenever`, which already had a serialize group, so if it is the same class the
aliasing must come from somewhere else. Treat it as an open, unexplained instance of the
general hazard above rather than as its own investigation: it is not reproducible, and
chasing it directly has already consumed several sessions with nothing to show.

## Reproduction harness

`news/2026-09/supply-act-serialization-and-the-concurrency-crash-cluster.md` records what
worked and what did not. The short version for whoever picks this up:

- **Do not start with `valgrind --tool=memcheck`.** It serializes threads onto one core
  and therefore suppresses the interleaving this class needs; it reported 0 errors on a
  workload that was demonstrably racing.
- **`helgrind` could not symbolize the binary** (`??? (in .../mutsu)`) or unwind an
  optimized frame, so its 1000 contexts were unusable. If a race detector is wanted,
  invest in ThreadSanitizer (`-Zsanitizer=thread`) rather than helgrind.
- **What worked** was running the real workload many times concurrently under the
  `gc-stress` environment and letting mutsu's own `Vec` bounds check fire. An
  AddressSanitizer build helps not by reporting anything but by slowing execution ~10x,
  which widens the window: the same file failed 2/36 under ASan versus 1/64 native.
