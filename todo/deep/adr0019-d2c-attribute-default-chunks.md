# ADR-0019 D2c is bigger than its ADR text implies — needs its own D2c-1/2/3 split

D2c ("Compile defaults/constraints as child chunks") reads in the ADR as a
contained follow-up to D2b: replace `default`/`is_default`/`where_constraint`
`Expr`s with `CompiledDeclExpr` run through `run_decl_expr`, "collapsing the
three near-duplicated env-setup blocks in `attr_build_defaults.rs`,
`methods_object_default_ctor.rs`, and `methods_object_dispatch_new.rs`." A
2026-08-07 research pass (after D2b/D2d landed — PRs #6024, #6025) found the
real footprint is substantially larger, and surfaced one architectural
prerequisite D2b didn't create. Recorded here rather than attempted directly,
following the project's measure-then-split precedent (C6 became nine PRs;
D2 itself is already subdivided a-d).

## What's bigger than advertised

**Eval sites**: not 3, but **≥15 distinct call sites across 10 files**,
falling into at least 5 different env-setup shapes:

- (A) "Full" setup — `attr_build_defaults.rs::eval_attr_default_expr`
  (lines 288-336): binds `self`, `?CLASS`, every already-set attribute as
  both `!name`/`.name`, switches `current_package`.
- (B) "Full + extra" setup — `methods_object_default_ctor.rs`'s inline block
  in `build_native_default_instance` (lines 203-275): same as (A) plus
  `__ANON_STATE__` and `constructing_class`, and its package switch is
  *conditionally* gated on `has_class_scoped_subs` where (A)'s is
  unconditional — this looks like accidental drift, not a deliberate
  difference, and is worth double-checking during implementation.
- (C) "Minimal" setup — `methods_object_dispatch_new.rs`'s per-class-attrs
  block (lines 2145-2162): only binds `self`, nothing else.
- (D) No setup — `methods_object_attr_constraints.rs::check_attribute_where_constraint`
  (the only `where_constraint` eval site), `construct_proxy_subclass`,
  `dispatch_new_and_constructors`'s `.bless` arm (`methods_dispatch_new.rs`),
  `registration_class_body_attr.rs`'s class-level-attr and `is_default` arms
  (both registration-time, no `self` — no instance exists yet),
  `registration_class_augment.rs`'s CStruct/NativeCall raw-bytes path.
- (E) `captured_env`-merge only (role composition) — `types/roles.rs`
  (mixin path) and `types/role_mixin_class.rs::seed_mixin_role_attributes`
  (fresh composition path).

Blocks (A)/(B) are ~90% structurally identical and a plausible shared
helper; (C) is a genuine simplification (skips sibling-attr/`::?CLASS`
visibility) that may or may not be intentional — needs raku-behavior
verification before merging it into a shared helper, not just assumed
equivalent.

**Type-swap sites**: ~25-30 more call sites read `.default`/`.where_constraint`
purely for their *type* — 9 `Expr::Literal` fast-path checks (the common
`= 0`/`= Nil` case skips the env dance entirely today) plus every
`ClassAttributeDef { .. }` construction/destructuring site.

## The architectural prerequisite D2b didn't create

`CompiledAttrDecl::from_stmt(&Stmt) -> CompiledAttrDecl` (landed in D2b,
`src/opcode.rs`) takes only `&Stmt`, no `&Compiler` — of its 4 call sites,
only the compiler's mainline/EVAL `has`-outside-class arm
(`compiler/stmt.rs`) runs at compile time. `class_body_has_decl`
(`registration_class_body_attr.rs`), `role_body_has_decl`
(`registration_role_body.rs`), and the augment arm
(`registration_class_augment.rs`) all call it at **registration time**,
walking `CompiledClassDeclPlan`/`CompiledRoleDeclPlan::legacy_body`. So
bolting `CompiledDeclExpr` onto `CompiledAttrDecl.default`/
`where_constraint`/`is_default` is not a type-swap for 3 of its 4 producers
— there's no `Compiler` in scope to call `compile_decl_expr` on.

The fix is architecturally supported by what's already there:
`Compiler::add_class_decl_plan`/`add_role_decl_plan`
(`compiler/decl_plan.rs:78-100`) already walk the same `body: &[Stmt]` at
compile time via `class_own_attribute_names`/`role_body_prescan`
(`opcode.rs:2215-2263`), in the same stable order the runtime has-arm walk
later re-visits. A parallel, index-aligned chunk vector could be built there
(promoting those free functions to `impl Compiler` methods, or adding a
sibling prescan) and threaded through the plan the same way `name_chunk`/
`own_attribute_names` already are — registration would then zip the
`legacy_body` walk against the pre-compiled chunks by position instead of
compiling fresh at registration.

**Keep a `Literal(Value)` fast path** (mirroring `DeclTraitArg`'s existing
3-variant shape) rather than collapsing straight to `Option<CompiledDeclExpr>`:
the 9 existing literal fast-paths exist specifically to skip env-setup/eval
overhead for the common case, and `runtime_init.rs`'s hardcoded builtin
`Proc` class attributes are built directly in Rust with no `Compiler` in
scope at all — a bare `CompiledDeclExpr` field would make that unpopulatable
without inventing a fake compiler pass for builtins.

## Role type-param substitution is env-level, not AST-level — confirmed compatible

Checked because it looked like a possible blocker: does a role attribute
default referencing the role's type param (`is default(T)`) need per-
consuming-class AST substitution, which a single compile-time
`CompiledDeclExpr` couldn't represent? No — `substitute_type_params_in_method`
(the role-method equivalent) only rewrites type-constraint *strings* in
`ParamDef`, never touches a method body's AST. Every attribute-default path
instead binds the type parameter as an **ordinary env variable** before
evaluation (`dispatch_new` restores `class_role_param_bindings` into
`self.env` before running defaults; `bind_role_type_params` does the same
for the native/bless ctor path). Since `compile_decl_expr` compiles chunks
with **no local slots** (every free variable resolves via dynamic env
lookup — same as today's raw `eval_block_value` calls), a `CompiledDeclExpr`
compiled once at the role's declaration site is fully compatible with this:
callers keep wrapping `run_decl_expr` in the same env-insert/run/restore
dance they already wrap `eval_block_value` in. `run_decl_expr` itself needs
no changes. This unblocks migrating `role_attribute_default_exprs`/
`role_class_level_attrs`/`class_attribute_default_exprs`
(`registry.rs:166,171,175`) the same way as ordinary class attributes.

## Recommended split for whoever picks this up

- **D2c-1** (`is_default` slice landed 2026-08-07, `default`/`where_constraint`
  still open): extend `add_class_decl_plan`/`add_role_decl_plan` to compile
  per-attribute `default`/`where_constraint`/`is_default` chunks alongside
  `own_attribute_names`, thread through the plans, change `CompiledAttrDecl`'s
  three fields to a `Literal(Value) | Compiled(CompiledDeclExpr)` shape
  (name it after `DeclTraitArg`'s pattern, or reuse `DeclTraitArg` itself —
  worth deciding during implementation), update its 4 call sites.
  **What actually landed**: only `is_default`, reusing `DeclTraitArg` as-is
  (its existing `Ast` variant absorbed the "not yet migrated" callers for
  free — no new fallback needed). `Compiler::add_class_decl_plan` now builds
  `CompiledClassDeclPlan::is_default_chunks: Vec<(Symbol, DeclTraitArg)>`,
  **keyed by attribute name** rather than position — this sidesteps the
  position-alignment risk called out above entirely (`class_body_has_decl`
  looks up its current `Stmt::HasDecl`'s name in the vec instead of relying
  on registration-time traversal order matching compile-time traversal
  order). The reason it stopped at `is_default` rather than all three
  fields: `default`/`where_constraint` are *stored* into
  `ClassAttributeDef` for later (construction-time) evaluation, so switching
  their type requires `ClassAttributeDef` to change in the same PR — that's
  D2c-2's work, described below unchanged. `is_default` is different: it is
  read-and-discarded once at registration time (`class_body_has_decl`
  evaluates it immediately via the new `eval_decl_trait_arg`, matching what
  `eval_block_value(&[Stmt::Expr(...)])` did before, just without the
  on-demand compile now that the common non-literal case is precompiled).
  Only 2 of the 4 `from_stmt` call sites ever read `.is_default`
  (`class_body_has_decl`, `role_body_has_decl`); the mainline/EVAL
  `has`-outside-class error path and `augment class` never did, so they
  needed no behavior change beyond passing `None` for the new
  `is_default_chunk` parameter. `role_body_has_decl` still stashes a raw
  `Expr` into `role_attribute_default_exprs` (D2c-3 territory) via a new
  `DeclTraitArg::as_expr()` escape valve. See
  `news/2026-08/d2c1-is-default-attribute-chunk.md`.
- **D2c-2**: change `ClassAttributeDef.default`/`where_constraint`
  (`src/runtime/mod.rs`) to match — it's copied straight from
  `CompiledAttrDecl` at registration, so this is a cheap follow-on once
  D2c-1 lands — and migrate the ~15 eval sites to `run_decl_expr`,
  collapsing env-setup blocks (A)/(B) into one parameterized helper (verify
  (B)'s `has_class_scoped_subs` gate and extra `__ANON_STATE__`/
  `constructing_class` bindings are intentional vs. drift before merging).
- **D2c-3**: migrate the three role registry tables
  (`role_attribute_default_exprs`/`role_class_level_attrs`/
  `class_attribute_default_exprs`) the same way — lower risk per the
  type-param finding above.

Each slice should get its own measured A/B (an env-var-gated force-compiled
instrument run against `make test` + targeted roast, per
[[feedback-measure-before-naming-the-fix]]-style verification) before
merging, given how many distinct env-setup shapes are in play and how easy
it would be to silently drop a binding one of the five shapes currently
provides.
