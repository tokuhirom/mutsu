# ADR-0019 D8-1: role deferred-body statements precompiled into typed ops

`CompiledRoleDeclPlan` gained `deferred_body_ops: Vec<DeferredBodyOp>`, one op
per `RoleBodyOp::Deferred` entry in D7-4's `body_plan` — reusing D7-4's
already-classified raw statements as input instead of re-deriving them from
`legacy_body`. Each `DeferredBodyOp` carries:

- `kind: TypeDecl | TokenRule | Plain`, mirroring
  `run_composed_role_deferred_body`'s own `is_type_decl`/`is_regex_decl`
  classification (a nested `class`/`role` registers under the role's own
  package at composition time; a `token`/`rule`/`regex` registers under the
  composing class's package, which isn't known until composition).
- `declared_vars: Vec<Symbol>`, replacing the runtime's own `VarDecl`
  re-scan for lexical-persistence bookkeeping — a non-`our`/non-`dynamic`
  `VarDecl`'s own name, empty for every other statement kind.
- `chunk: Option<CompiledDeclExpr>`, compiled against the role's own
  qualified package for `TypeDecl`/`Plain` statements. `TokenRule`
  statements keep `chunk: None` — the same ADR-0009 carve-out D6/D9 apply to
  class-body token/rule statements — and stay on the `run_block_raw` path.

`register_role_decl` copies the ops onto a new `RoleDef::deferred_body`
field. This slice is purely additive: `deferred_body_stmts` remains the sole
execution path, and nothing reads `deferred_body` back yet — the consumer
cutover (behind the design doc's raku-verified case tables for the "frozen
plan" question) is D8-2.

Pinned by a new compiler unit test
(`role_declarations_precompute_deferred_body`), verified via the full `t/`
suite (28,037 tests).
