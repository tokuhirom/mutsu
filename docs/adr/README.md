# Architecture Decision Records (ADR)

This directory records mutsu's architectural decisions.

## Purpose

For design forks in the road (major mechanism selections, ordering decisions, judgments that could be reversed),
make it possible to trace **"why we decided that way" and "what we rejected"** after the fact.
The role of an ADR is to preserve the *context of the judgment* — something that cannot be read out of the code or PLAN.md.

## Conventions

- 1 decision = 1 file. `NNNN-kebab-title.md` (sequential numbering).
- **Status**: `Proposed` (under discussion / awaiting approval) / `Accepted` (final) / `Superseded by ADR-XXXX` (updated).
- When a decision changes, **do not rewrite the existing ADR** — supersede it with a new ADR and update the old ADR's Status.
- **Record implementation progress inside the ADR that owns the decision** — a Status suffix
  for a short state, or an "Outcome" / "Implementation status" section for a phased one.
  `news/` and PLAN.md are where the *work* is reported; they are not a substitute. An ADR
  whose recorded state has drifted from what shipped defeats its own purpose: a reader who
  starts from the ADR either re-litigates a decision that is already executed, or cannot tell
  which of its phases are done. (The 2026-08-02 ledger review in
  [ANALYSIS.md §8](../../ANALYSIS.md) found this drift on six of seventeen ADRs.)
- Written in English (repo-wide English-only documentation rule).

## Index

| # | Title | Status |
|---|---|---|
| [0001](0001-gc-strategy-and-phasing.md) | GC adoption — mechanism selection and phasing | Accepted (layers 3a/3b/4 shipped; outcome in §7) |
| [0002](0002-phase-a-gate-reassessment.md) | Phase A gate reassessment — confirming the preconditions for starting GC | Accepted |
| [0003](0003-default-on-gc-trigger.md) | Trigger policy for default-on GC (synchronous + buffer-size threshold + adaptive backoff) | Accepted |
| [0004](0004-jit-strategy.md) | JIT — mechanism selection and phasing (Cranelift method JIT, no deopt) | Accepted |
| [0005](0005-nanbox-representation-encoding.md) | NaN-boxing representation switch (3b-1) — encoding choice and newtype-seal integration | Accepted |
| [0006](0006-baseline-interpreter-optimizations.md) | Baseline (classical) interpreter optimizations — adoption decisions and priorities | Accepted |
| [0007](0007-grammar-parse-trail-matcher.md) | Grammar/regex matcher — cursor + undo-log (trail) to kill capture-threading churn | Accepted |
| [0008](0008-push-based-supply-event-delivery.md) | Push-based supply event delivery (ReactWaker sinks) | Accepted |
| [0009](0009-regex-code-assertion-execution-model.md) | Regex code assertions — run inline in the real interpreter, and keep LTM declarative | Accepted |
| [0010](0010-cross-thread-lexical-sharing-scope.md) | Cross-thread lexical sharing is scoped to a spawn lineage, not the process | Accepted |
| [0011](0011-rakuast-model-layer-and-phasing.md) | RakuAST — a reflection/model layer over the internal AST, and its phasing | Accepted (Phases 1–5 landed; Phase 6 open) |
| [0012](0012-libffi-macos-arm64-vendored-bump.md) | libffi on macOS arm64 — bump the vendored build, do not switch to system libffi | Accepted |
| [0013](0013-container-interior-mutability-cellvalue.md) | Container interior mutability — kill the `gc_contents_mut` provenance UB with a `GcCell` newtype | Accepted (primitive landed; Miri gate outstanding — §8) |
| [0014](0014-make-test-runs-tap-on-debug-binary.md) | `make test` runs the TAP (`t/`) suite on the debug binary, not release | Accepted |
| [0015](0015-native-backed-container-storage-and-repr-bodies.md) | Native-backed container storage and synthesised REPR bodies (`BODY_OF`) | Accepted (P0–P3b landed; P3c open) |
| [0016](0016-span-based-captures-and-lazy-match.md) | Span-based regex captures and lazily materialized `Match` objects | Accepted (P1–P5 all landed) |
| [0017](0017-cli-option-errors-follow-rakudo.md) | A command-line *option* error follows rakudo — message, stream, and a zero exit status | Accepted |
| [0018](0018-slot-addressed-lexical-capture-and-env-sync.md) | Slot-addressed lexical capture and env synchronization | Accepted |
| [0019](0019-compiled-declarations-and-unified-method-dispatch.md) | Compile declarations and unify method dispatch entries | Proposed |
| [0020](0020-shared-worker-pool.md) | Shared worker pool — elastic growth, blocking `await` | Accepted (all slices landed; per-task clone slimming tracked separately) |
| [0021](0021-argument-namedness-is-a-call-site-property.md) | Argument named-ness is a call-site property — Pair flavour unification | Accepted (P1-P3a and P3 shipped; P4/P5 remain) |
| [0022](0022-regex-alternation-ltm-ranking.md) | `\|` alternation ranks branches by declarative-prefix LTM, not by longest actual match | Proposed (design complete; implementation not started) |
| [0023](0023-binding-provenance-spawn-capture.md) | Spawn-time capture ownership is decided by binding provenance, not value type | Accepted (implemented) |
| [0024](0024-mainline-lexicals-for-named-subs.md) | Mainline is a compunit — named subs resolve mainline free variables through unit-lexical cells | Accepted (implemented) |
| [0025](0025-captured-scalar-cells-value-kind-blind.md) | Cell boxing of captured scalars must be value-kind-blind — retiring the Instance skip | Accepted (slice 1 implemented; slices 2-3 planned) |
| [0026](0026-slang-activation-architecture.md) | Slang activation — bundle Slangify + Slang::Tuxic verbatim, map recognized grammar-mixin overrides onto parser modes | Accepted |
| [0027](0027-loop-frozen-value-capture-cascade.md) | Loop-frozen value captures cascade through nested closure creation — frame-owned vouching gated on the live value kind | Accepted (Slice 1 implemented; Slices 2-3 planned) |
| [0028](0028-supply-schedule-on-deferred-tap-delivery.md) | `Supply.schedule-on` genuinely defers tap delivery — callback shims at the tap-registration chokepoint, with a serialized per-tap drain | Proposed |
| [0029](0029-exception-class-role-membership.md) | Built-in `X::` exception ancestry is role membership, not a single parent — register it through the existing composed-role path | Accepted (Slices 1-3 + residue R1-R4 implemented; Slice 4's real-`Test` sweep tracked separately — see "Implementation status") |
| [0030](0030-native-array-decode-cache-interior-mutability.md) | The native `array[T]` decode cache is a read-path cache, and needs field-level interior mutability — not `gc_contents_mut` | Accepted (implemented in full) |
| [0031](0031-supply-quit-ownership-and-cold-source-tapping.md) | A supply block's quit belongs to its own emitter, and a cold `whenever` source is tapped rather than replayed | Partially implemented (Slice 1 shipped; Slices 2-3 open) |
| [0032](0032-wrapvarref-container-capture-across-closure-boundaries.md) | `WrapVarRef` container capture is a property of the capture edge, not of the named-sub declaration form | Proposed (design complete; implementation not started) |
| [0033](0033-whatever-priming-leaf-and-derived-scope.md) | Whatever-priming is a leaf property plus a derived scope — defer `WhateverCode` construction out of the parser | Proposed (design complete; implementation not started) |
| [0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md) | Reifying a `Seq` fills the Seq itself — reification and consumption are two operations, not one | Proposed (design complete; implementation not started) |
| [0031](0031-supply-quit-ownership-and-cold-source-tapping.md) | A supply block's quit belongs to its own emitter, and a cold `whenever` source is tapped rather than replayed | Proposed (design complete; implementation not started) |
| [0032](0032-wrapvarref-container-capture-across-closure-boundaries.md) | `WrapVarRef` container capture is a property of the capture edge, not of the named-sub declaration form | Proposed (design complete; implementation not started) |
