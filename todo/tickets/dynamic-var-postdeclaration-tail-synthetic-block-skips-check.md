# A tail-position `:=`-bind dynamic-var declaration can skip X::Dynamic::Postdeclaration / X::Dynamic::Package entirely

Found 2026-08-21 while fixing
`dynamic-var-read-before-later-inner-my-star-decl-false-postdeclaration.md`
(the false-positive ticket: an outer read wrongly poisoning an unrelated
inner `my $*x` declaration). That fix made `accessed_dynamic_vars`
lexically scoped (reset on `push_dynamic_scope_lexical`, restored on
`pop_dynamic_scope_lexical`) and, while verifying the genuine-error case
still fires, surfaced a separate, pre-existing gap: when a `my $*x := ...`
declaration (parsed as `Stmt::SyntheticBlock([MarkReadonly, VarDecl])`, the
wrapper the parser uses for a `:=` bind) ends up as the block-final /
tail-position / expression-position statement, several compile sites
recursively call `compile_block_inline(inner)` (or an equivalent) on the
wrapper's *inner* statements to get "declare and yield the value" tail
semantics. That recursive call reaches `Stmt::VarDecl` through a **separate,
hand-inlined tail-position compile arm** (in `compile_block_inline`,
`compile_unit`'s mainline loop, and the `compile_expr`/do-stmt-value paths)
that never had the `X::Dynamic::Postdeclaration` / `X::Dynamic::Package`
checks the *ordinary* (non-tail) `Stmt::VarDecl` arm in `stmt.rs` has always
had.

The false-positive fix added a shared helper,
`Compiler::check_dynamic_var_decl_errors` (in
`src/compiler/helpers_dynamic.rs`), and wired it into the one tail-position
`VarDecl` arm the ticket's own verification example exercised
(`helpers_block_inline.rs`'s `compile_block_inline`, used for e.g. `do {
say $*CUR; my $*CUR := 42; }`), plus a `next_dynamic_scope_inline_transparent`
one-shot flag so that recursive inlining call does not reset
`accessed_dynamic_vars` mid-check (a `Stmt::SyntheticBlock` is never a real
lexical scope; its direct, non-tail dispatch already inlines with no
push/pop at all). That closes the specific case the task asked to verify.

**Other call sites were NOT patched and still skip the checks entirely** for
a tail-position `:=`-bind of a dynamic variable:

- `src/compiler/mod.rs` (`compile_unit`'s top-level mainline loop, ~line
  3162-3192): a top-level program whose LAST statement is `my $*x := ...`
  after an earlier `$*x` read anywhere in the mainline. Minimal repro
  (currently prints `x` instead of dying):
  ```raku
  say $*POSTDECL // "x"; my $*POSTDECL := 1;
  ```
  (raku: `===SORRY!=== ... Illegal post-declaration of dynamic variable
  '$*POSTDECL' ...`)
- `src/compiler/expr_block.rs` lines ~717-793 (`compile_expr`'s
  `Stmt`-as-expression dispatch — the `SyntheticBlock` arms for
  `MarkSigillessReadonly`/`MarkBind`/bound-array-len/destructuring wrappers,
  each documented as "compile it inline (NOT scope-isolated) so the `my`
  leaks into the enclosing scope").
- Possibly others reachable via `compile_block_inline`/`compile_do_block_expr`
  recursion — `src/compiler/expr_data.rs`, `helpers_phasers.rs`,
  `helpers_control_flow.rs`, `stmt.rs:4318` were not individually audited.

## Why this is a separate ticket, not folded into the fix above

The false-positive ticket is about the check firing when it **should not**;
this is the opposite defect (the check **not firing** when it should) in a
disjoint set of compile paths, discovered only as a side effect of
verifying the fix's true-positive case. Auditing and patching every
`compile_block_inline`-reachable tail-position `VarDecl` site is a wider,
more careful sweep than the original ticket's scope, and risks widening the
blast radius of an otherwise narrow, well-tested fix.

## Suggested approach for whoever picks this up

1. For each call site above, determine whether it is (a) inlining a
   `Stmt::SyntheticBlock`'s already-flattened statements (should be
   scope-transparent, like the site already fixed) or (b) a genuine new
   scope (e.g. a real `do {}`/sub body — should keep resetting
   `accessed_dynamic_vars` normally).
2. For (a) sites, set `self.next_dynamic_scope_inline_transparent = true;`
   immediately before the recursive `compile_block_inline(...)` call
   (mirroring `helpers_block_inline.rs`'s fixed call site), and call
   `Compiler::check_dynamic_var_decl_errors(name)` in whatever hand-inlined
   tail-position `VarDecl` compile arm applies there (mirroring the fix in
   `helpers_block_inline.rs`'s `compile_block_inline`).
3. Consider whether a more structural fix is worth it instead of touching
   every call site individually — e.g. making `Stmt::SyntheticBlock` recognizable
   to `push_dynamic_scope_lexical` itself (a depth/kind marker rather than a
   one-shot flag set at each call site), so a *future* new call site cannot
   silently reintroduce this gap.
4. Add regression tests per site fixed, mirroring
   `t/dynamic-var-postdeclaration-scope-boundary.t`.
