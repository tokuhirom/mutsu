# Chained comparison has no AST node, so RakuAST renders the expanded `&&` / `DoBlock`

mutsu's parser expands a chained comparison at parse time instead of representing it.
`src/parser/expr/precedence/chain_cmp.rs` has two expansions:

- **pure middle** — `build_chain_cmp_expr_with_repeated_middle` rewrites `a < m < b` into
  `(a < m) && (m < b)`, *duplicating* `m`;
- **effectful middle** — `build_chain_cmp_expr` emits an `Expr::DoBlock` that binds a
  `__mutsu_chain_cmp_N` temporary and then conjoins.

`src/parser/expr/precedence/comparison.rs` synthesizes the same conjunction for the mixed
and negated chaining paths.

Runtime semantics are correct in both shapes (the middle is evaluated exactly once), so
this is purely a *representation* gap. Its visible cost is RakuAST fidelity:

```
$ raku  -e 'say Q[1 < 2 < 3].AST'   # ApplyInfix(ApplyInfix(1,"<",2), "<", 3)
$ mutsu -e 'say Q[1 < 2 < 3].AST'   # RakuAST::StatementPrefix::Do  (the DoBlock shape)
```

Rakudo has no AST-level `&&` here at all — the chaining semantics come from the
operator's *chaining precedence* at code-gen. ADR-0033's "Phase-4 prerequisite" section
proposed `Expr::ChainedCompare { operands, ops }` to close this, noting it would let
Phase 2 render `1 < * < 10` faithfully rather than as the expanded form.

## Why this is a separate ticket rather than part of ADR-0033 Phase 4

Phase 4 needed only that the synthesized conjunction be *distinguishable* from a
user-written `&&` (so that `&&` could become a Whatever-priming thunk barrier while
`1 < * < 10` stayed a single priming scope). It got that from a dedicated
`TokenKind::ChainAnd`, which keeps the `Expr::Binary` shape and therefore needed no
changes in any expression walker — a bounded 20-site audit. See ADR-0033's "Phase 4
outcome" section.

A new `Expr` variant is the expensive half and buys only the rendering fidelity. Phase 1
of the same ADR needed `Expr::WhateverCurry` arms in **41 files**, and the failure mode is
silent rather than a compile error: any walker with a `_ => {}` catch-all — placeholder
collection (`collect_ph_expr_shallow`, which must see `{ $^a < $^b < $^c }`), sink
warnings, closure free-variable analysis, `outer_redecl`, `whenever_scope` — would simply
stop seeing the chain's operands, with no diagnostic.

## Shape of the work

1. Add `Expr::ChainedCompare { operands: Vec<Expr>, ops: Vec<(TokenKind, bool)> }`,
   produced by `chain_cmp.rs` / `comparison.rs` instead of the expanded form.
2. Defer the expansion to the compiler, exactly as ADR-0033 Phase 1 did for
   `Expr::WhateverCurry`: a `compile_expr` arm that calls the existing
   `build_chain_cmp_expr*` helpers and compiles the result, so emitted bytecode is
   unchanged. Note `build_chain_cmp_expr` synthesizes `Stmt::VarDecl` and uses the global
   `CHAIN_CMP_TMP_COUNTER`, which moves with it.
3. Audit every `Expr` walker for a missing arm. Grep for the files that took a
   `WhateverCurry` arm in ADR-0033 Phase 1 — that list is a good proxy.
4. `src/rakuast/convert.rs`: render it as rakudo's left-nested `ApplyInfix` chain.
5. Once the node exists, `TokenKind::ChainAnd` can be retired along with the
   `exprs_structurally_eq` middle-duplication detector in
   `src/whatever_curry/{build,replace}.rs` — with a real node there is no duplication to
   undo. That cleanup is the main non-cosmetic gain.

## Pins that must keep passing

`t/whatever-chained-comparison.t`, `t/whatever-thunky-operators.t` (its last four
assertions are exactly the chain-vs-`&&` distinction, dual-oracle against raku), and
`t/rakuast-whatever-code.t`.

## Re-verified 2026-08-26 (deferred, not started)

Re-checked against `raku` v2026.06 while landing the sibling parser tickets in
`t/parser-expression-gaps.t`. Everything above still holds, with two
clarifications worth having before someone picks this up:

- **Runtime semantics are already correct and were re-confirmed**, so nothing
  user-visible is broken: `1 < m() < 3` evaluates the middle exactly once in
  both implementations, and `5 < q1() < 100` short-circuits after the first
  comparison in both (middle ran once, second comparison skipped). This is
  purely the RakuAST rendering gap the ticket describes.
- **The `Q[1 < 2 < 3]` example in the ticket body is slightly off.** Even the
  all-literal chain — where the "pure middle" duplication path should apply —
  renders as the `StatementPrefix::Do` temp-binding shape in mutsu today, not as
  a duplicated `&&`. rakudo renders it as the plain left-nested
  `ApplyInfix(ApplyInfix(1,"<",2), "<", 3)` with no `&&` and no block, exactly
  as described. So the divergence is the DoBlock shape in both the pure and the
  effectful case.

Deferred deliberately: this is the expensive half (a new `Expr` variant plus the
41-file walker audit ADR-0033 Phase 1 needed, with a silent failure mode for any
walker carrying a `_ => {}` catch-all) and buys only rendering fidelity, so it
did not belong in a batch of behavioural parser fixes.
