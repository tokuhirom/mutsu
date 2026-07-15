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
| [0007](0007-grammar-parse-trail-matcher.md) | Grammar/regex matcher — cursor + undo-log (trail) to kill capture-threading churn | Proposed |
