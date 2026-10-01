# ADR-10499: Rewriting passes walk the AST through one exhaustive mutable visitor

- **Status**: Accepted (maintainer decision, 2026-10-01; implemented — see "Implementation
  status")
- **Date**: 2026-10-01
- **Related**: [ADR-0137](0137-typed-ast-visitor-for-analyses.md) (the read-only visitor, which
  deferred this decision), [ADR-0133](0133-no-per-call-ast-compile-at-runtime.md) (`ParamCode`),
  [#10468](https://github.com/tokuhirom/mutsu/issues/10468),
  [#10499](https://github.com/tokuhirom/mutsu/issues/10499)

## Context

ADR-0137 gave analyses one exhaustive, typed, read-only visitor and left a mutable variant to "a
separate decision when the first rewriting pass wants one". Porting the remaining hand-rolled
walkers (#10468) reaches the rewriting passes. About sixteen recursive walkers change the tree, in
two styles:

- **in place**, over `&mut Stmt` / `&mut Expr`: the outer-redeclaration check, which also tags a
  self-initializing declaration (`parser/outer_redecl/`); the nested-`BEGIN` lift
  (`runtime/begin_prologue/nested.rs`); the phaser lift and reorder (`runtime/phasers.rs`); the
  WhateverCode marking pass (`whatever_curry/mark/`);
- **by value**, `fn(&Expr) -> Expr`, rebuilding every node it does not change by hand: the `{*}`
  proto-dispatch rewrite (`runtime/dispatch_proto_rewrite.rs`), the WhateverCode placeholder
  replacement (`whatever_curry/replace.rs`), the `supply` body rewrite
  (`parser/primary/ident/supply.rs`) and the loop-phaser rewrites in
  `compiler/helpers_phasers.rs`.

Every one of them carries its own `match` with a `_ =>` arm. A variant or field added to the AST is
a compile error in `ast_visit`'s walkers and in none of these, so a construct a rewrite should have
reached is silently left alone — the same failure ADR-0137 removed for analyses. The by-value ones
are also long (up to ~230 lines each) mostly because they reconstruct every untouched variant field
by field.

## Decision

1. **`trait VisitMut`** in `src/ast_visit/` with `visit_stmt_mut(&mut Stmt)`,
   `visit_expr_mut(&mut Expr)`, `visit_param_mut(&mut ParamDef)` and
   `visit_regex_node_mut(&mut RegexNode)`, and matching `walk_*_mut` functions that supply the
   default recursion into every child. The `walk_*_mut` functions obey ADR-0137 §1: every variant
   destructured with every field named, no `_ =>` arm and no `..` rest pattern. There is no
   `visit_name_mut`: a rewrite that renames does so in the hook of the node that owns the name.
2. **No by-value `Fold`.** A by-value rewrite becomes a clone of the input followed by an in-place
   `VisitMut` pass. Those rewrites already clone every node they do not change, so the cost is the
   same, and one mutable visitor keeps the number of exhaustive walks at two.
3. **`ParamCode` is reset by the walk.** `walk_param_mut` gives a parameter's default, `where` and
   shape expressions to the visitor and then replaces the parameter's `ParamCode` with a fresh,
   empty slot (`ParamCode::default()`), so a rewriting pass cannot leave the compiled chunks of the
   old expression attached (ADR-0133: "code that rewrites one of those expressions must reset
   it"). Rewriting passes run before the routine is compiled, so the slot is normally empty
   already; resetting it is the safe default rather than a cost.
4. **Ordering stays in the visitor's hands.** A pass that must see a statement list as a list — to
   insert, remove or reorder statements, or to process declarations before later statements — does
   that in its `visit_stmt_mut` / list handling and calls `walk_*_mut` for the children it does not
   restructure. A pass whose *semantics* is to visit only some positions overrides the hooks for
   the positions it skips, with a comment saying why (the ADR-0137 porting rule).

## Consequences

- Adding an AST variant or field is a compile error in two places (`walk_*` and `walk_*_mut`)
  instead of being skipped by every rewriting pass; both are mechanical.
- The by-value rewriters shrink to their real cases.
- A rewrite that newly reaches a child it used to skip can change behaviour; each port checks those
  positions against `raku` and pins them in tests, as ADR-0137's porting rule requires.
- `scripts/ast-walkers-baseline.txt` stops listing the rewriting passes as they are ported; what
  remains are code generation, single-path spine recursions and the by-design partial walks.

## Rejected alternatives

- **Keep the rewriting passes hand-rolled** (the third close condition of #10468). Leaves the
  silent-skip failure in exactly the passes whose misses change program behaviour rather than a
  diagnostic.
- **A by-value `Fold` as well as `VisitMut`.** A third exhaustive walk to keep in step for no
  capability the clone-then-mutate form lacks.
- **A generic visitor over `&T` / `&mut T`** (one macro-generated walk for both). Possible, but the
  read-only walk reports typed names (`visit_name`) that a mutable walk does not need, and a macro
  makes the exhaustive destructuring harder to read and review than two plain functions.

## Implementation status

- **Slice 1**: `trait VisitMut` and the `walk_*_mut` functions (`src/ast_visit/visit_mut.rs`,
  `walk_mut_{stmt,expr,decl}.rs`). Beyond §1 the trait has one more hook,
  `visit_stmts_mut(&mut Vec<Stmt>)`: every walk hands a statement list to it, which is the
  "list handling" of §4 (an ordered pass keeps its list loop there; a lift edits the list).
  Ported: the outer-redeclaration scope walk (`parser/outer_redecl/`), the nested-BEGIN lift
  (`runtime/begin_prologue/nested/walk.rs`), the phaser lift and reorder recursion
  (`runtime/phasers/lift.rs`); 176 → 166 walkers. Copies of executed code are skipped by every
  lift: the `target`/`rhs` of a `CompoundAssign` (a model-layer copy of `expanded`), and a regex
  tree (#10550).
- **Slice 2**: the rest of the rewriting passes. The WhateverCode leaf classifier's statement
  walk (`whatever_curry/mark/stmt.rs`); the placeholder replacement (`whatever_curry/replace.rs`,
  by value → clone + `VisitMut`, the numbered and `$_` forms unified); the proto `{*}` rewrite
  (`runtime/dispatch_proto_rewrite.rs`, by value → clone + `VisitMut`); the supply-body
  `emit`/`done` rewrite (`parser/primary/ident/supply.rs`, merged with its expression twin);
  the loop-exit phaser wrapping and the ENTER-expression hoist (`compiler/loop_exit_phasers.rs`).
  166 → 158 walkers. What stays hand-rolled in the baseline is code generation, lowerings,
  single-path spines, one-scope statement-list transforms (the BEGIN prologue partition, the
  prelude tagging) and `replace.rs`'s priming-scope spine, each with its note. The parser-side
  `nested_block_methods.rs` hoist is left to the parser cluster of #10468.
