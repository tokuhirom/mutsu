# ADR-0002: Phase A gate reassessment — confirming the preconditions for starting GC

- **Status**: Accepted
- **Date**: 2026-07-03
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0001](0001-gc-strategy-and-phasing.md) (this ADR supplements and finalizes the judgment criteria for
  ADR-0001 §3-2's "start GC only after Phase A is complete". It does not retract or supersede ADR-0001 itself)

## 1. Context

ADR-0001 decided "start GC after Phase A (catching up to raku on compatibility + speed) is complete",
but delegated the judgment of "complete" to PLAN.md's metrics table (whitelist count, perf ratios).
When evaluating on 2026-07-03 whether to start GC development, that table turned out to be **stale**.

## 2. Findings

- The whitelist count recorded in PLAN.md, "1285 / target 1300+", is an old value.
- Measured: whitelist **1345** (`roast-whitelist.txt`) / **1463** total roast `.t` files.
  → **The 1300+ target is already met.** The remaining ~118 files are, as §F describes, a long tail with
  little prospect of individual fixes: syntax rakudo itself has not implemented, MoarVM REPR dependencies, fudge targets, etc.
- The perf targets (method-call <1.5x etc.) are not met, but the remaining major levers (Lever 2 NaN-boxing, Lever 4 JIT) are
  **ordered after GC (layer 3b, layer 4) by ADR-0001 itself**. Waiting for perf completion before starting GC would be
  the circular condition "make completion of post-GC work a precondition for starting GC" — self-contradictory.
  The only remaining pre-GC perf item is Lever 3 (threaded dispatch), which can proceed in parallel, independent of GC.

## 3. Decision

- **Fix the Phase A completion criterion as "substrate complete ∧ roast target met."** Completion of all perf levers is
  not part of the Phase A completion criterion (Levers 2/4 come after GC by design).
- By the above criterion, **Phase A is deemed complete, and development of GC (Phase B) begins.**
- Update PLAN.md's phase table and metrics table to the numbers in this ADR.
- Implementation follows the procedure in `docs/gc-level1-detailed-design.md` §11, starting with **root visitor introduction**
  (consolidating roots into `Interpreter::visit_roots()`; the GC proper and `Gc<T>` are not yet implemented).

## 4. Consequences

- Update PLAN.md's "A. Catch up (we are here)" to "B. Value-representation rework + GC (we are here)".
- The remaining roast/perf items (§F long tail, Lever 3 onward) continue in parallel. They are not mutually exclusive with starting GC.
- Whenever PLAN.md's metrics table is updated in the future, measure at that time (e.g. `wc -l roast-whitelist.txt`) rather than
  keeping stale numbers on record (as in this case, they distort judgment).
