# Parser Improvement Plan (`parser`)

This document defines the ongoing parser refactoring policy and next steps.

Detailed opportunity audit: `docs/parser-opportunities-audit.md`.

## Goals

- Improve change safety for parser work.
- Reduce duplicated operator/statement matching logic.
- Make parser behavior easier to extend and review.
- Keep behavior stable while refactoring internals.

## Core policy

1. **Type first, macro last**
   - Prefer typed enums and small helper functions.
   - Introduce macros only after patterns are stable and repetitive.
2. **No behavior changes in refactor-only PRs**
   - Refactor and feature work should be split when possible.
3. **Single ownership per grammar concern**
   - Each grammar concern should have one parser entry path.

## Current direction (active)

### A. Typed operator parsing

Keep replacing string-branch duplication with typed operator layers:

- `ComparisonOp` for comparison parsing.
- `CompoundAssignOp` for `+=`, `//=`, etc.
- `ConcatOp` / `AdditiveOp` / `MultiplicativeOp`.
- `PrefixUnaryOp` / `PostfixUpdateOp`.

Expected pattern:

1. Parse text into `enum Op`.
2. Map `Op -> TokenKind`.
3. Build AST using the mapped token.

### B. Shared parsing helpers

Prefer helper functions for recurring patterns:

- `parse_*_op(input) -> Option<(Op, len)>`
- keyword-with-boundary checks
- delimited argument list parsing

### C. Parse diagnostics as structured data

Keep parse diagnostics both human-readable and machine-friendly:

- Use `RuntimeErrorCode` (`ParseUnparsed`, `ParseExpected`, `ParseGeneric`) instead of raw tag strings.
- Populate `RuntimeError { code, line, column }` at parser boundary (`parser::parse_program`).
- Keep `message` useful for humans (`near` snippet, line/column text), but do not rely on message parsing in tooling.

## Next milestones

### Milestone 1: Expression parser completion

- Type and factor remaining repeated operator checks in `expr.rs`.
- Keep precedence layering unchanged.

### Milestone 2: Packrat-oriented error strategy

- [x] Track and report **furthest failure position** across alternatives.
- [x] Aggregate expected-token categories at furthest point for clearer diagnostics.
- [x] Surface parse errors with structured metadata (`code`, `line`, `column`) plus readable messages.
- [ ] Expand furthest-error aggregation to remaining niche branches where fallback still masks specific failure causes.

### Milestone 3: Packrat-oriented memoization strategy

- [x] Introduce opt-in memoization for high-backtracking entry points (`statement`, `expression`, `primary`).
- Start with `(rule_id, input_offset)` cache keys and validate memory/latency tradeoffs.
- Keep default parser behavior unchanged unless perf data shows clear benefit.

### Milestone 4: Test expansion

- Add targeted parser unit tests for each extracted helper.
- Add prove tests only for user-visible behavior.

### Milestone 5: Error/context improvements

- [x] Standardize parse error context generation.
- [x] Align `near` snippet trimming with reported column.
- [x] Expose parse metadata in CLI output (`code`, `line`, `column`).
- [ ] Add a compact user-facing “hint” layer for common parser mistakes (without losing raw expected messages).

## Refactor checklist

When making parser refactors:

1. Keep parser behavior unchanged unless explicitly intended.
2. Add or adjust focused parser unit tests.
3. Run `cargo test parser::`.
4. Run full `cargo test` before merge.
5. Update `docs/parser-overview.md` if structure changed.

For Packrat-oriented work specifically:

6. Measure parse time before/after on representative inputs.
7. Validate memoization cache hit/miss ratio and peak memory.
8. Add tests for furthest-error reporting correctness.

## Out of scope (for now)

- Reintroducing parser backend switching.
- Macro-heavy parser DSL.
- Large grammar redesign in one PR.
