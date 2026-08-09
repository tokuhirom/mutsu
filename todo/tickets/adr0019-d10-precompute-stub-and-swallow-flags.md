# ADR-0019 D10 follow-up: precompute two boolean flags instead of matching raw Stmt

D10 (deleting the class/role AST registration walkers) closed by amending its
completion criterion to accept typed ops carrying their raw statement as an
opaque payload for one-shot field extraction — the same shape the ADR's own
C6 precedent blessed for `FunctionDef.body`. See the D10 entry in
`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`.

Two of the six accepted raw-`Stmt` reads are not payload extraction but a
cheap boolean *decision*, and could be precomputed at compile time to
slightly harden the invariant further (this is optional polish, not
required — filed so it isn't lost, not because it blocks anything):

1. **`walk_role_body`'s `RoleBodyOp::Deferred` stub-marker check**
   (`src/runtime/registration_role_decl.rs`): pattern-matches
   `raw.as_ref()` for `Stmt::Expr(Expr::Call { name, .. })` where `name` is
   `__mutsu_stub_die` or `__mutsu_stub_warn`, to set `cx.role_def.is_stub_role
   = true`. This is a single boolean fact about the statement, knowable at
   compile time the same way `CompiledRoleDeclPlan::is_stub` (ADR-0019
   D7-1/D9-1) already is for the *whole role body*. Add an
   `is_stub_marker: bool` (or similar) field to `RoleBodyOp::Deferred`,
   computed once in `classify_role_body_stmt` (`src/opcode.rs`), and have
   the registration-time check read that instead of re-matching `raw`.

2. **`class_body_other_stmt`'s BEGIN/EVAL-swallow shape check**
   (`src/runtime/registration_class_body.rs`): the `is_swallowable`
   local — `matches!(stmt, Stmt::Phaser { kind: Begin, .. } | Stmt::Call {
   name: "EVAL", .. } | Stmt::Expr(Expr::Call { name: "EVAL", .. }))` — and
   the related `is_compile_time_phaser` check (`Begin | Check`) are both
   pure functions of the raw statement's shape, knowable at compile time.
   Add the equivalent boolean(s) to `ClassBodyOp::Other`/`ClassSub`, computed
   in `classify_class_body_stmt`, so the registration-time arm reads a flag
   instead of re-deriving it from `raw`.

Neither is required reading `raw`'s actual *content* beyond the
already-classified op's discriminant — genuine payload extraction (e.g.
`ProtoMethod`'s param defs, `LeavePhaser`'s inner body, the anon-method
attribute validation inside `class_body_other_stmt`) is out of scope here
and is the accepted permanent shape per D10's closing note.

Low priority: this does not change behavior, does not unblock any other
work, and the current code is correct — it just isn't the *smallest*
possible AST-shape footprint. Do opportunistically if touching these files
for another reason, otherwise skip.
