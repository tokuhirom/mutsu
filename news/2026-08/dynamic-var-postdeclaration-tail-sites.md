# Close the remaining tail-position `X::Dynamic::Postdeclaration` gaps

A follow-up to the earlier scope-boundary fix
(`news/2026-08/dynamic-var-postdeclaration-scope-boundary.md`): that fix
patched the ONE call site its own verification example exercised
(`compile_block_inline`'s own tail-position `VarDecl` arm), and recorded the
rest in
`todo/tickets/dynamic-var-postdeclaration-tail-synthetic-block-skips-check.md`
as a follow-up sweep. This closes that ticket.

A tail-position `my $*x := ...` declaration (a `:=` bind — parsed as
`Stmt::SyntheticBlock([MarkReadonly/MarkBind, VarDecl])`) reached through
several OTHER compile paths still silently skipped the
`X::Dynamic::Postdeclaration` / `X::Dynamic::Package` checks entirely,
because each path hand-inlines a block-final/tail-position/expression-
position `VarDecl` through its own separate compile arm rather than the
ordinary (non-tail) `Stmt::VarDecl` arm that has always had the checks.

```raku
say $*POSTDECL // "x"; my $*POSTDECL := 1;
```

- raku: `===SORRY!=== ... Illegal post-declaration of dynamic variable
  '$*POSTDECL' ...`
- mutsu (before this fix): printed `x`, silently accepting the illegal
  post-declaration.

## Audit and fix

Every `compile_block_inline`-reaching call site in `src/compiler/` was
audited and classified as either (a) inlining a `Stmt::SyntheticBlock`'s
already-flattened statements — a parser wrapper, never a real lexical scope,
so it must stay transparent to the enclosing block's dynamic-var read
tracking — or (b) a genuine new scope (a real `do {}`/sub/phaser/if-branch
body), which correctly keeps resetting `accessed_dynamic_vars`. Six
previously-unpatched (a) sites were fixed:

- `compile_unit`'s top-level mainline tail-statement dispatch
  (`src/compiler/mod.rs`) — a top-level program's last statement.
- `compile_expr`'s four `Stmt::SyntheticBlock` arms in expression position
  (`src/compiler/expr_block.rs`) — sigilless-readonly marks, `:=` binds,
  bound array-length markers, and list/hash destructuring.
- `given`/`when`/`default` tail-statement dispatch
  (`compile_when_tail_stmt` in `src/compiler/helpers_block_inline.rs`).
- Value-collecting block-body dispatch (`compile_stmts_value` in
  `src/compiler/helpers_control_flow.rs`) — used by e.g. `do for ... { }`.
- A routine's own body tail-statement dispatch, three sites in
  `src/compiler/helpers_sub_body.rs` — a fresh `Compiler` compiles each
  routine body and, like the top-level mainline, never pushes its own
  dynamic-var scope around the body itself, so it had the identical gap.
- The `let`-block / phaser-block-scope tail-statement dispatch
  (`compile_tail_stmt_value` in `src/compiler/stmt.rs`).

## Structural fix, not just per-site patches

Rather than repeating the one-shot
`next_dynamic_scope_inline_transparent = true;` flag inline at every call
site (as the original fix did at its one site), the fix adds a shared
`Compiler::compile_synthetic_block_inline` helper (in
`src/compiler/helpers_dynamic.rs`) that sets the flag and calls
`compile_block_inline` together. Every site identified above — including the
original already-fixed one — now goes through this helper, so a future call
site that needs to inline a `SyntheticBlock` gets the correct transparent
behavior by construction instead of needing to remember the one-shot-flag
pattern.

Other pure AST-traversal / static-analysis sites that pattern-match
`Stmt::SyntheticBlock` (declaration hoisting in `decl_plan.rs`, phaser
rewriting in `helpers_phasers.rs`, variable-name extraction, block-local
detection) do not call `compile_block_inline` and were confirmed
uninvolved.

Verified against `raku` as the oracle for every fixed site's true-positive
case, plus regression guards that an unrelated read inside a closed nested
`do {}` still does not leak into and poison a genuinely unrelated tail `:=`
bind through any of the newly-fixed paths (mirroring the false-positive
regression coverage of the original fix). New regression coverage:
`t/dynamic-var-postdeclaration-tail-sites.t` (8 subtests).
