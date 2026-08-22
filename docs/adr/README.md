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
| [0019](0019-compiled-declarations-and-unified-method-dispatch.md) | Compile declarations and unify method dispatch entries | Accepted/Implemented (all completion gates closed 2026-08-17; non-gating residue tracked separately) |
| [0020](0020-shared-worker-pool.md) | Shared worker pool — elastic growth, blocking `await` | Accepted (all slices landed; per-task clone slimming tracked separately) |
| [0021](0021-argument-namedness-is-a-call-site-property.md) | Argument named-ness is a call-site property — Pair flavour unification | Accepted (P1-P3a and P3 shipped; P4/P5 remain) |
| [0022](0022-regex-alternation-ltm-ranking.md) | `\|` alternation ranks branches by declarative-prefix LTM, not by longest actual match | Accepted (all five slices implemented and merged) |
| [0023](0023-binding-provenance-spawn-capture.md) | Spawn-time capture ownership is decided by binding provenance, not value type | Accepted (implemented) |
| [0024](0024-mainline-lexicals-for-named-subs.md) | Mainline is a compunit — named subs resolve mainline free variables through unit-lexical cells | Accepted (implemented) |
| [0025](0025-captured-scalar-cells-value-kind-blind.md) | Cell boxing of captured scalars must be value-kind-blind — retiring the Instance skip | Accepted (slice 1 implemented; slice 2 closed 2026-08-20 as already resolved by existing machinery; slice 3 planned) |
| [0026](0026-slang-activation-architecture.md) | Slang activation — bundle Slangify + Slang::Tuxic verbatim, map recognized grammar-mixin overrides onto parser modes | Accepted (implemented — Slangify/Slang-Tuxic bundled verbatim 2026-08-11, unblocking the Text::CSV battery; see "Outcome") |
| [0027](0027-loop-frozen-value-capture-cascade.md) | Loop-frozen value captures cascade through nested closure creation — frame-owned vouching gated on the live value kind | Accepted (Slice 1 implemented; Slices 2-3 planned) |
| [0028](0028-supply-schedule-on-deferred-tap-delivery.md) | `Supply.schedule-on` genuinely defers tap delivery — callback shims at the tap-registration chokepoint, with a serialized per-tap drain | Accepted (Slice 1 implemented and Cro-verified 2026-08-13; Slice 2 audited) |
| [0029](0029-exception-class-role-membership.md) | Built-in `X::` exception ancestry is role membership, not a single parent — register it through the existing composed-role path | Accepted (Slices 1-3 + residue R1-R4 implemented; Slice 4's real-`Test` sweep tracked separately — see "Implementation status") |
| [0030](0030-native-array-decode-cache-interior-mutability.md) | The native `array[T]` decode cache is a read-path cache, and needs field-level interior mutability — not `gc_contents_mut` | Accepted (implemented in full) |
| [0031](0031-supply-quit-ownership-and-cold-source-tapping.md) | A supply block's quit belongs to its own emitter, and a cold `whenever` source is tapped rather than replayed | Implemented (Slices 1-3 shipped 2026-08-19) |
| [0032](0032-wrapvarref-container-capture-across-closure-boundaries.md) | `WrapVarRef` container capture is a property of the capture edge, not of the named-sub declaration form | Partially implemented (Slice 1+2 landed 2026-08-19; Slice 3 open) |
| [0033](0033-whatever-priming-leaf-and-derived-scope.md) | Whatever-priming is a leaf property plus a derived scope — defer `WhateverCode` construction out of the parser | Accepted (Phase 1 shipped 2026-08-19; Phase 2 shipped 2026-08-20; Phases 3-4 not implemented) |
| [0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md) | Reifying a `Seq` fills the Seq itself — reification and consumption are two operations, not one | Accepted (phases 1-4 implemented; phase 5 Miri probes deferred — see §7.1) |
| [0035](0035-method-calls-observe-caller-frames.md) | Caller-frame observation from method bodies — chain-aware dynamics enumeration, plus `uses_callframe`-gated frame pushing at the two compiled-method chokepoints | Accepted (Slices 1-3 implemented; see "Implementation status") |
| [0036](0036-element-container-pairs-from-subscripts-and-pairs.md) | A Pair produced by a subscript adverb or `.pairs` carries the element *container*, not a snapshot — retiring the `self.env` value-equality search | Partially implemented (Slices 1-2 landed 2026-08-20; Slices 3-4 open) |
| [0037](0037-eval-context-frame-owns-the-return-target.md) | `EVAL ..., context => $frame` — the context frame owns the return target, and the routine chain must be dispatch-path-independent | Partially implemented (Slice 1 landed 2026-08-20; Slices 2-4 open) |
| [0038](0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md) | `.cache` returns a `List`, and the Seq/List view is a property of the value — read through one oracle | Proposed (design complete; implementation not started) |
| [0039](0039-container-lexicals-resolve-lexically.md) | `@`/`%` lexicals must resolve lexically — retiring by-name container resolution (ADR-0024's deferred sigil follow-up) | Proposed (Slice 1 landed 2026-08-20; Slice 2 next) |
| [0040](0040-array-hash-elements-are-itemized-at-the-store.md) | Array and Hash elements are itemized at the *store*, not compensated at the read | Proposed (design complete; implementation not started) |
| [0041](0041-sub-hoisting-vs-compile-time-name-visibility.md) | A sub's callability is hoisted for the whole scope, but `&name` bareword-reference visibility at `constant`/`BEGIN` time must follow textual order | Proposed (investigation only) |
| [0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md) | A type constraint belongs to the container, not to a name — retiring the `var_type_constraints` side table | Partially implemented (Slice 1 landed 2026-08-20; Slices 2-3 not started) |
| [0043](0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md) | The scheduled-delivery hop belongs to the tapped Supply — every derived live operator carries `"scheduler"` forward | Proposed (Decision 1 verified and ready to implement; Decision 2 deferred behind a recorded trigger) |
| [0044](0044-listops-are-routines-not-a-syntactic-rewrite.md) | Core listops (`push`/`pop`/`splice`/…) are routines, not a syntactic rewrite — give them a callable core candidate | Proposed (design complete; implementation not started) |
| [0045](0045-for-loop-parameters-bind-the-element-container.md) | A `for` loop parameter binds the element *container*; the per-iteration writeback is retired | Proposed (design complete; implementation not started) |
| [0046](0046-proto-token-ltm-shares-one-ranking-mechanism.md) | Proto-token dispatch shares the one LTM ranking mechanism, and interpolation provenance covers arrays and token bodies | Partially implemented (Slice 1 landed 2026-08-20; Slices 2-5 next) |
| [0047](0047-type-identity-is-a-declaration-site-not-a-registry-name.md) | A type's identity is its declaration site, not its current registry name — retiring `subtest`'s registry rollback | Partially adopted (P1/P2 landed, PR #6757; P3/P4 not started) |
| [0048](0048-placeholder-scope-is-a-block-invocation-contract.md) | Placeholder scope is a per-construct block-invocation contract, not a per-AST-arm boundary flag | Accepted (P1 landed; P2-P5 not started) |
| [0049](0049-nil-decays-to-the-container-default-at-the-element-store.md) | `Nil` decays to the *container's* default at the element store, and stops being a hole sentinel | Accepted (Slices 0-2 implemented) |
| [0050](0050-block-routine-ness-is-a-definition-site-property.md) | A Block's routine-ness is a definition-site lexical property, not a re-derived dynamic one | Proposed (design complete; implementation not started) |
| [0051](0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md) | Type ancestry has one oracle, and an unresolved method throws instead of stringifying | Accepted (P1/P3/P4 landed; P2/P5 not started) |
| [0052](0052-a-when-clause-produces-its-value-on-the-stack.md) | A `when`/`default` clause produces its value on the stack, in both branches — retiring the succeed-signal and side-channel value paths | Proposed (design complete; implementation not started) |
| [0053](0053-do-whenever-produces-a-tap-on-the-stack.md) | `do whenever` produces a `Tap` on the stack — retiring the source-variable name bridge | Proposed (design complete; implementation not started) |
| [0054](0054-argument-list-interpolation-is-a-call-site-property.md) | Argument-list interpolation is a call-site property — retire blind Slip flattening | Accepted (Slices 1-2 implemented; Slices 3-6 remain) |
| [0055](0055-closure-free-vars-resolve-to-their-own-binding.md) | A closure's free variable resolves to its own captured binding — retiring `merge_all` and the two closure-state stores | Proposed (design complete; implementation not started) |
| [0056](0056-nativecall-types-display-only-qualification.md) | NativeCall's `Pointer`/`CArray`/`long`/... display under `NativeCall::Types::*` — display-only, registry key stays bare | Accepted (implemented) |
| [0057](0057-var-reflection-identity-cell-address.md) | `.VAR` reflection identity is the shared cell's address, not a per-frame cache — reusing ADR-0032's container-capture edge as the boxing trigger | Accepted (implemented) |
| [0058](0058-map-grep-produce-a-deferred-seq.md) | `.map`/`.grep` produce a deferred `Seq` — the callback runs at first consumption, not at the call | Proposed (design complete; implementation not started) |
