# ADR-0003: Trigger policy for default-on GC (the level-1a production trigger)

- **Status**: Accepted (user approval 2026-07-05)
- **Date**: 2026-07-05
- **Relates to**: [ADR-0001](0001-gc-strategy-and-phasing.md) (§4.2 activation mechanism / §4.3 A' scope),
  [ADR-0002](0002-phase-a-gate-reassessment.md), `docs/gc-level1-detailed-design.md` §9

## 1. Context

The level-1a cycle collector (Bacon-Rajan on Arc) is functionally complete: emission of all
safepoint kinds (#4195), cooperative cross-thread STW under worker churn (#4205), the
phantom-edge unsoundness fix for shared captured-envs (#4213), a complete deterministic/random
stress surface (§9.2), and making every CI gc-stress step blocking (#4219). However, GC is
**still default off**, and even with `MUTSU_GC=on`, without an automatic trigger configured,
collection happens exactly once at program end — a long-running server process keeps
accumulating cycle garbage. ADR-0001 left §4.2 (synchronous vs asynchronous) and
§4.3 (how much of A' to assume as a prerequisite) undecided.

Two facts established on 2026-07-05 constrain the design:

1. **A push-count-periodic trigger is asymptotically unsuitable for production**.
   `MUTSU_GC_EVERY_CANDIDATE=N` fires a full trial-deletion scan (cost = O(live suspect graph))
   every N pushes. On a workload that churns candidates while growing a large live structure
   (S17-lowlevel/cas-loop.t: building a linked list via 4×1000 `cas`), N=64 yields
   8500+ collects × ~4300 nodes traced each = **over 180 seconds** (N=1024: 3.3 seconds,
   GC-off ~5 seconds). A fixed period re-scans the same surviving suspects over and over.
   The trigger must adapt to "how much did the previous scan actually reclaim".
2. **A' (root consolidation) is NOT a prerequisite for level-1a** (per the §11 step 11
   investigation): the collector is refcount-driven and consumes no root enumeration at all,
   so consolidating frame/upvalue roots contributes nothing to either the 1a trigger or its
   correctness. ADR-0001 §4.3 is resolved narrowly as "A' is deferred".

## 2. Decision (Proposed)

1. **Synchronous collect** (ADR-0001 §4.2 = synchronous; reconfirming the detailed design's
   §12 "1a drops the background collector"). The trigger arms the existing `PENDING` flag,
   and the collect runs inline on the next mutator that hits a safepoint (with the existing
   cooperative STW if other mutators are alive). Asynchronous (concurrent) collection is
   rejected for 1a: trial deletion cannot tolerate concurrent refcount mutation (it would
   need an epoch mechanism), and with the full safepoint coverage in place, the
   trigger-to-collect latency is already small enough.

2. **Trigger = candidate-buffer "size" threshold + adaptive backoff**.
   `buffer_candidate` arms `PENDING` when the buffer length reaches the effective threshold.
   After each collect, update `threshold = clamp(BASE, 2 × survivors, MAX)`
   (survivors = number of live suspects that were revived): a scan where "almost everything
   was alive" raises the threshold (automatic escape from the cas-loop-style quadratic),
   while a scan where "almost everything was reclaimed" resets it to BASE. The only tunable
   is `MUTSU_GC_THRESHOLD` (BASE; default to be determined by measurement — starting point
   16384). The existing stress env variables remain as-is for CI/debugging (the production
   trigger is an independent, additional mechanism).

3. **Flipping the `MUTSU_GC` default from off to on** happens only after satisfying, in order:
   a. gc-stress roast made blocking and kept green (done in #4219; continued observation).
   b. Threshold-trigger implementation + assurance via gc_stress (byte-identical output vs
      GC-off; bounded `pause_ns_max` under churn; cas-loop-style workloads within ~1.2x of
      GC-off).
   c. Measured overhead budget: fib/int loops ≈ 0 (the scalar type filter — ADR-0001's
      "push == 0 on fib" gate); method-call / bench-class < 5% vs GC-off.
   d. The opt-out `MUTSU_GC=off` is retained.
   The flip itself happens after this ADR is Accepted, in a PR accompanied by the actual
   measurements for b/c.

4. **A' is deferred** (per Context item 2): treated on its own timeline, as an enabler for
   future root-consuming VERIFY extensions and for NaN-boxing / JIT-generation optimizations.

## 3. Consequences

- Server-style programs (workers that never join and churn cycles — ADR-0001's motivating
  case) get automatic cycle reclamation proportional to buffer growth, without the quadratic
  pathology.
- The `MUTSU_VM_STATS` gc counters become the acceptance instrumentation for the default-on
  flip.
- The stress knobs (`EVERY_SAFEPOINT`/`EVERY_CANDIDATE`/`AT`/random) are unchanged, for
  CI/debugging use.

## 4. Alternatives considered

| Option | Verdict |
|---|---|
| Push-count period (reusing `EVERY_CANDIDATE`) | Rejected — periodic firing of the O(live graph) scan is quadratic (cas-loop.t measured at over 180 seconds) |
| Concurrent collect on a background thread | Rejected (for 1a) — trial deletion does not tolerate concurrent mutation. Reconfirms the detailed design's §12 |
| Time-based (every N seconds) | Rejected — wasteful on idle processes, too slow under bursts. A size threshold naturally tracks load |
| Requiring A' completion as a prerequisite | Rejected — the collector consumes no root enumeration (§11 step 11 investigation) |

---

*Accepted by user approval on 2026-07-05. Implementation follows the Decision in §2; the
default-on flip happens once the gates in §2-3 are met.*

## 5. Update (2026-07-05 — acceptance measurements at flip time and gate revision)

Measurements for §2-3c (release build, best of 9 iterations, load < 1.5):

| bench | GC-on overhead | Verdict |
|---|---|---|
| fib25 / bench-fib / int-arith | +0.5% / +3.0% / +2.0% | ✓ (with the type filter, scalar/function hot paths are nearly free — achieving ADR-0001's original intent) |
| method-call | +0.7–6.4% (measurement noise band) | ✓/△ |
| **bench-class** | **+7.5–8.3%** | ✗ (against < 5%) |
| churn suite | ≤ +10% | ✓ (≤ 1.2x) |

The bench-class residual is a structural cost: one buffered-bit branch per drop layered on
the pre-existing clone traffic of "16.5M `Value` clone/drop operations against 28k
instances". The cheap GC-side improvements have already landed (drop fast path taking
+61%→+8%, skipping the program-end collect, caching `gc_enabled`). Reducing the traffic
itself is the territory ADR-0001 assigns to layer 3b (NaN-boxing).

**User decision (2026-07-05): accept the ~8% on bench-class and flip to default-on as-is.**
The value of a leak-free default outweighs 8% on class-dense code, and the root-cause
reduction will be recovered in 3b. The only exception is `cfg!(test)` (the crate's own
unit-test builds), which stays default off — for test isolation under parallel `cargo test`
(GC-on unit testing is covered by the CI gc-stress job).
