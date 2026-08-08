# ADR-0019 D6-1: class-body static names precomputed, redundant `TrustsDecl` walk arm deleted

The first slice of D6 ("Remove `CompiledClassDeclPlan::legacy_body`") lands two independent,
cheap facts identified by the 2026-08-08 D6/D9 design pass
(`todo/deep/adr0019-d6-d9-legacy-body-removal.md`):

- **`CompiledClassDeclPlan::declared_static_names: Vec<Symbol>`** — the names a class body
  `my`/`state`-declares at its own top level, precomputed at plan lowering by a new
  `class_declared_static_names` free function that mirrors `persist_class_body_statics`'s
  inline scan (a top-level, unflattened `Stmt::VarDecl` that is neither `our` nor `dynamic`).
  `persist_class_body_statics` now takes this precomputed slice instead of re-walking the raw
  body on every class registration to decide which lexicals count as class-body statics.
- **The redundant `Stmt::TrustsDecl` walk arm in `run_class_body` is deleted.**
  `publish_class_shell` already inserts the same `class_trusts` entry from D1's `trusts` plan
  field *before* the body walk starts, so the walk arm was a pure double-insert into the same
  `HashSet`. The compiler already compiles a bare `TrustsDecl` statement to a no-op
  (`compiler/stmt.rs`), so the statement now safely falls through to the catch-all
  `class_body_other_stmt` arm with no observable behavior change.

Both changes are pure mechanical hoists — no new fallback, no behavior change — verified with a
new compiler unit test (`class_declarations_precompute_declared_static_names`) plus the existing
`t/trusts-undeclared.t`, `t/private-trusts.t`, `t/class-body-static-in-sub.t`,
`t/class-body-my-lexical-scope.t`, and related class-body-static tests, and a full `make test`
run.

D9-1, the role-side twin (role `is_stub` + our-scope-violation plan facts), is a separate slice.
