# Parser Opportunities Audit (2026-02)

This document records concrete parser improvement opportunities found by reviewing the current `parser_nom` implementation.

## Summary

- The parser works and current targeted tests pass.
- There is still substantial maintainability debt, mostly from parser size and repeated branch-style matching.
- The biggest risk is regression from changes in large files with long ordered dispatch chains.
- Packrat-specific concerns (memoization and furthest-error handling) are not yet implemented in a systematic way.

## High-priority opportunities

## 1) Split oversized parser files

Current file sizes:

- `src/parser_nom/stmt.rs`: 2687 lines
- `src/parser_nom/primary.rs`: 1491 lines
- `src/parser_nom/expr.rs`: 1271 lines

Repository convention says Rust source files should stay under 500 lines.

### Why this matters

- Hard to review safely.
- Hard to reason about dispatch ordering.
- High merge conflict probability across concurrent parser work.

### Recommendation

Refactor into focused modules while preserving public entry points:

- `stmt/decl.rs`, `stmt/control.rs`, `stmt/loop.rs`, `stmt/call.rs`, `stmt/dispatch.rs`
- `expr/binary.rs`, `expr/unary.rs`, `expr/postfix.rs`, `expr/comparison.rs`
- `primary/literal.rs`, `primary/quote.rs`, `primary/var.rs`, `primary/collection.rs`

## 2) Typed statement dispatch table (status update)

This has been implemented (statement parser dispatch table exists now).

### Why this matters

- Ordering bugs are easy to introduce accidentally.
- Adding new statements is noisy and error-prone.
- Hard to inspect and test ordering as data.

### Acceptance criteria

- Dispatch order stays byte-for-byte equivalent to current behavior.
- Dedicated tests verify precedence-sensitive statement conflicts.

## 3) Normalize operator tables across all expression layers

Good progress already exists (`ComparisonOp`, `CompoundAssignOp`, etc.), but coverage is partial.

### Remaining gap

- Some operator detection still uses ad-hoc `starts_with` branching in place.
- Boundary checks (identifier continuation rules) are repeated in multiple helpers.

### Recommendation

- Complete typed operator extraction for all operator families.
- Centralize shared boundary checks for keyword-like operators.

## Medium-priority opportunities

## 4) Packrat-style memoization for backtracking hotspots

Current parser logic is recursive-descent with frequent alternative attempts, but there is no parse-result memoization cache.

### Why this matters

- Repeated parsing of the same `(rule, offset)` can create avoidable overhead.
- As grammar coverage grows, backtracking cost can grow non-linearly on difficult inputs.

### Recommendation

- Add opt-in memoization for selected high-traffic rules first:
  - `statement()`
  - `expression()`
  - `primary()`
- Cache key shape: `(rule_id, byte_offset)`.
- Cache value shape: success/failure plus resulting offset and error metadata.
- Gate behind feature flag or runtime switch initially; measure before enabling by default.

## 5) Packrat-style furthest-error reporting

Current error handling is mostly local `expected(...)` messages plus top-level remaining-input snippet.

### Why this matters

- In PEG/Packrat-style parsing, best diagnostics usually come from the furthest failure point.
- Without furthest-position tracking, users often see generic or misleading early-branch errors.

### Recommendation

- Track furthest failure offset across alternates.
- Aggregate expected token categories at that offset.
- Emit diagnostics from furthest failure, not first failure.

## 6) Introduce parser context object

Current parser functions pass raw `&str` repeatedly and recompute context.

### Recommendation

Introduce lightweight `ParseCtx` for:

- byte offset and line cache
- shared keyword boundary helpers
- standardized error snippets

This does not require changing external parse API initially.

## 7) Improve parse error diagnostics consistency

Top-level errors include a useful snippet, but deeper failures are still generic.

### Recommendation

- Add consistent expected-token categories.
- Normalize snippet length and escaping.
- Include nearest grammar layer (`statement`, `expression`, `primary`) in messages.

## 8) Expand parser-focused regression tests

There are unit tests, but some high-risk grammar edges remain underrepresented.

### Additions recommended

- dispatch-order conflict tests (`known_call_stmt` vs `assign_stmt` vs `expr_stmt`)
- operator precedence matrix tests (table-driven)
- postfix/index/method interaction cases
- whitespace-sensitive call/listop distinctions

## Low-priority opportunities

## 9) Parser benchmark harness

Add a lightweight benchmark command (or criterion target) for:

- parse throughput on representative roast/local test files
- before/after comparison for refactors

## 10) Property/fuzz testing for parser robustness

Use fuzz/property tests against parser entry points for panic/regression detection.

## Suggested execution order

1. File split (`stmt` first, then `expr`, then `primary`)
2. Finish operator table normalization
3. Add furthest-error tracking
4. Add selective memoization (with benchmarks)
5. Error diagnostics normalization
6. Regression test matrix expansion
7. Optional perf/fuzz work

## Notes

- This audit is about maintainability and development velocity; not all items require behavior change.
- Prefer small, isolated PRs with parser unit tests per step.
