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
- Written in English (repo-wide English-only documentation rule).

## Index

| # | Title | Status |
|---|---|---|
| [0001](0001-gc-strategy-and-phasing.md) | GC adoption — mechanism selection and phasing | Accepted |
| [0002](0002-phase-a-gate-reassessment.md) | Phase A gate reassessment — confirming the preconditions for starting GC | Accepted |
| [0003](0003-default-on-gc-trigger.md) | Trigger policy for default-on GC (synchronous + buffer-size threshold + adaptive backoff) | Accepted |
| [0004](0004-jit-strategy.md) | JIT — mechanism selection and phasing (Cranelift method JIT, no deopt) | Accepted |
| [0005](0005-nanbox-representation-encoding.md) | NaN-boxing representation switch (3b-1) — encoding choice and newtype-seal integration | Accepted |
| [0006](0006-baseline-interpreter-optimizations.md) | Baseline (classical) interpreter optimizations — adoption decisions and priorities | Accepted |
| [0007](0007-grammar-parse-trail-matcher.md) | Grammar/regex matcher — cursor + undo-log (trail) to kill capture-threading churn | Accepted |
| [0008](0008-push-based-supply-event-delivery.md) | Push-based supply event delivery (ReactWaker sinks) | Accepted |
| [0009](0009-regex-code-assertion-execution-model.md) | Regex code assertions — run inline in the real interpreter, and keep LTM declarative | Accepted |
| [0010](0010-cross-thread-lexical-sharing-scope.md) | Cross-thread lexical sharing is scoped to a spawn lineage, not the process | Accepted |
| [0011](0011-rakuast-model-layer-and-phasing.md) | RakuAST — a reflection/model layer over the internal AST, and its phasing | Accepted |
| [0012](0012-libffi-macos-arm64-vendored-bump.md) | libffi on macOS arm64 — bump the vendored build, do not switch to system libffi | Accepted |
| [0013](0013-container-interior-mutability-cellvalue.md) | Container interior mutability — kill the `gc_contents_mut` provenance UB with a `GcCell` newtype | Proposed |
