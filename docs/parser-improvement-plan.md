# Parser Improvement Plan (`parser_nom`)

This document defines the ongoing parser refactoring policy and next steps.

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

## Next milestones

### Milestone 1: Expression parser completion

- Type and factor remaining repeated operator checks in `expr.rs`.
- Keep precedence layering unchanged.

### Milestone 2: Statement dispatch cleanup

- Replace long `if let Ok(...)` chain in `statement()` with a typed dispatch table.
- Preserve current dispatch order exactly.

### Milestone 3: Error/context improvements

- Standardize parse error context generation.
- Keep existing user-visible messages unless intentionally changed.

### Milestone 4: Test expansion

- Add targeted parser unit tests for each extracted helper.
- Add prove tests only for user-visible behavior.

## Refactor checklist

When making parser refactors:

1. Keep parser behavior unchanged unless explicitly intended.
2. Add or adjust focused parser unit tests.
3. Run `cargo test parser_nom::`.
4. Run full `cargo test` before merge.
5. Update `docs/parser-overview.md` if structure changed.

## Out of scope (for now)

- Reintroducing parser backend switching.
- Macro-heavy parser DSL.
- Large grammar redesign in one PR.
