# ADR-0018: Slot-addressed lexical capture and precise environment synchronization

- **Status**: Proposed
- **Date**: 2026-08-02
- **Deciders**: tokuhirom, Codex
- **Related**: [ADR-0001](0001-gc-strategy-and-phasing.md), [ADR-0010](0010-cross-thread-lexical-sharing-scope.md), [ADR-0013](0013-container-interior-mutability-cellvalue.md), [ANALYSIS.md](../../ANALYSIS.md) §1.2/§1.3/§2.4, [lexical-scope-slot campaign](../lexical-scope-slot-campaign.md)

## Context

Compiled lexical reads and writes are slot-addressed, but several runtime mechanisms still
observe or publish those lexicals through the name-keyed `Env`. `CompiledCode` currently
sets `captures_env_by_name` when it contains any `ForLoop`, `BlockScope`,
`BlockLocalScope`, `MakeGather`, or `WheneverScope`. That one bit overrides the existing
per-slot `needs_env_sync` analysis and makes every local an env-mirror target.

The blanket is load-bearing because the five consumers do not yet carry a complete account
of the slots they observe. Block exit has historically recovered mutations by name, loop
machinery publishes its parameter and control state through env, gather and whenever execute
stored bodies against a live env, and closure construction falls back to materializing that
env. Removing only the blanket deterministically breaks all four mechanism families.

The mirror is also unsound as lexical identity. Two simultaneously-live shadowed bindings
with the same name collapse to one env key. A nested call can therefore replace a closure's
capture with a same-named callee parameter, authoritative whenever captures can leak into an
ambient env, and cross-thread publication needs masks and rollback rules to reconstruct a
lexical distinction the key cannot represent.

## Decision

The local slot is the authoritative identity and storage location for every compiled lexical.
`Env` remains the compatibility store for genuinely name-based facilities, but it is not a
second authoritative copy of a frame.

### Per-consumer slot sets

`CompiledCode` will record the precise local slots required by each of the five consumers.
The compiler/finalization scan computes those sets from opcode operands, nested compiled-code
free variables, loop parameters and control carriers, and stored-body metadata. Their union,
plus reflective access, drives `needs_env_sync`; the presence of a consumer does not fill the
whole vector.

Every consumer must use its own set when publishing to or importing from env. A missing slot
in static metadata is conservative for that consumer only: it may request a frame-wide sync
at the boundary while the migration is Proposed, but it may not silently widen every store in
the frame. The temporary conservative escape hatch is deleted before this ADR becomes
Accepted.

### Block restore

`BlockScope` and `BlockLocalScope` restore lexical state by slot identity. A block records the
slots it owns and the outer slots it may mutate. On exit it clears/restores only owned slots
and leaves mutations to outer slots in place. It must not reconstruct locals by looking up
`code.locals[slot]` in env.

Phaser execution uses the same slot map. Exceptional exits, `LEAVE`/`KEEP`/`UNDO`, `let`,
`temp`, and `$OUTER::` must observe the same binding identities as normal exits.

### Closure capture cells

An escaping mutable lexical is captured as one shared `ContainerRef` cell addressed by its
creator-frame slot. The creator slot, closure upvalue and any cross-thread descendant share
that cell. Mutation analysis is only an optimization hint: when mutability cannot be proven
absent, capture uses a cell. This follows ADR-0001's soundness rule and avoids snapshots whose
correctness depends on incomplete mutation analysis.

A proven immutable capture may remain an indexed by-value upvalue. Aggregate sigils and rw
sinks do not opt out of cell capture merely because a generic path is currently
`ContainerRef`-blind; those paths must be made cell-aware as part of the migration.

Closure invocation resolves indexed captures before any ambient name lookup. Env-by-name is
retained only for reflective features (`EVAL`, symbolic dereference, `CALLER::`) and system or
dynamic names that are not ordinary lexical slots.

### Thread sharing

ADR-0010's spawn-lineage store remains the compatibility mechanism during migration. Once a
captured lexical has a shared cell, thread clones carry the cell by slot-derived capture
metadata and do not publish that lexical under its bare name. Process-wide internal
`__mutsu_*` keys remain process-wide as ADR-0010 requires.

## Migration

1. Add regression pins for the env-writeback correctness cluster and counters for blanket and
   per-consumer synchronization.
2. Introduce per-consumer slot metadata without changing runtime behavior.
3. Convert `ForLoop`, `MakeGather`, and `WheneverScope` boundaries to publish only their slot
   sets; convert `BlockScope` and `BlockLocalScope` restore to slot identity.
4. Make closure creation and invocation use slot-addressed shared cells, including thread
   clones and rw/container mutation sinks.
5. Remove `captures_env_by_name`, the whole-vector `needs_env_sync.fill(true)`, and closure
   whole-env fallback for ordinary lexicals.
6. Close the related `todo/` findings only when their reproductions are pinned and pass, then
   change this ADR to Accepted.

Each step is independently buildable and testable. Ordered implementation layers use stacked
PRs; no intermediate layer may rely on a higher layer to restore correctness.

## Consequences

- Shadowed lexicals retain distinct identities across blocks, closures, calls and threads.
- Block cleanup cannot leak or resurrect bindings through a same-named env entry.
- Closure creation becomes proportional to its captures instead of the size of the ambient
  environment once the final fallback is removed.
- Reflective name access still pays an explicit synchronization cost. That boundary is honest:
  reflection asks for names, while ordinary compiled execution uses slots.
- The migration touches compiler analysis, scope execution, closure dispatch and concurrency.
  The wide deterministic regression surface is handled by targeted pins plus CI roast,
  gc-stress and jit-stress jobs, not by retaining the unsound blanket.

## Rejected alternatives

### Keep `captures_env_by_name` and add local fixes

Rejected. Every fix becomes another consumer of the name mirror and preserves collisions by
construction.

### Snapshot captured values and write them back on return

Rejected. Separately registered methods, rw arguments and concurrency can mutate a capture in
ways the current static analysis cannot prove. Copy-out also has no sound ordering for
concurrent writers.

### Eagerly box every lexical

Rejected. It would make every scalar access pay container indirection and would expand the
`ContainerRef`-blind surface unnecessarily. Boxing occurs at escape/capture boundaries; an
uncertain escaping capture is boxed, while a non-escaping local stays a plain slot value.

