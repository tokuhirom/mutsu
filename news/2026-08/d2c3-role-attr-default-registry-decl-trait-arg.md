# ADR-0019 D2c-3: role attribute-default registry tables run through `DeclTraitArg`

Follow-on to D2c-2 (`news/2026-08/d2c2-attribute-decl-trait-arg.md`): the three
`Expr`-valued registry tables that carry a role attribute's deferred default across
composition — `role_attribute_default_exprs`, `role_class_level_attrs`, and
`class_attribute_default_exprs` (`src/runtime/registry.rs`) — are now `DeclTraitArg`-valued,
matching `ClassAttributeDef.default`/`.where_constraint`.

The write side actually got simpler. `registration_role_body.rs` used to convert
`decl.is_default` — already a `DeclTraitArg` since D2c-1 — back into a raw `Expr` via the
`DeclTraitArg::as_expr()` escape valve just to store it into `role_attribute_default_exprs`.
It now stores the `DeclTraitArg` directly, retiring that escape valve's only caller outside
an `Ast`-only path. `role_class_level_attrs` still wraps `CompiledAttrDecl.default` (still
`Option<Expr>` — out of scope for D2b/D2c) in `DeclTraitArg::Ast`, the same pattern D2c-2
used at `ClassAttributeDef`'s own construction sites.

`registration_class_compose.rs` (the role→class copy that runs at composition) and all four
eval sites the migration touched — `runtime_var_meta.rs`'s
`class_attribute_default_with_role_fallback` and `apply_container_attribute_defaults`, and
`methods_call_dispatch.rs`'s role type-object class-level-attribute read (`R.x` on a role's
type object) — now call `Interpreter::eval_decl_trait_arg` instead of
`eval_block_value(&[Stmt::Expr(...)])`. The `methods_call_dispatch.rs` site is a fourth eval
site the original D2c research pass's enumeration missed: it was found by grepping the
registry table names directly, which surfaces every reader regardless of which eval
mechanism it happened to use, rather than repeating the original field-by-field code search.

With D2c-1 through D2c-3 landed, no `ClassAttributeDef` or role-registry attribute-default /
`where`-constraint path in the interpreter still evaluates through a raw `Expr` plus its own
`eval_block_value` call. Verified the same way as D2c-2: the full `t/` suite (2935 files,
27795 tests) and every roast-whitelisted `S12-attributes`/`S14-roles` file, all green.

What's left under the parent D2c box is the actual bytecode precompilation — every
`DeclTraitArg` these paths build is still `Literal`/`Ast`, never `Compiled`, so a non-literal
default's expression still compiles fresh on every construction, same as before D2c-1
through D2c-3. That, and D2d (accessor publication), are the remaining open boxes in ADR-0019
Phase D2.
