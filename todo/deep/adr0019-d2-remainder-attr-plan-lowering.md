# ADR-0019 D2 remainder design: plan-lowered attribute descriptors and compiled default/where chunks

Design pass (2026-08-08, no code landed) for the two open pieces of the D2 box: the D2b
remainder (compiler-lowered `Vec<CompiledAttrDecl>` on the plans) and the D2c remainder
(actually using `DeclTraitArg::Compiled` for `default`/`where_constraint`, which today still
recompile per construction through the `Ast` variant). Builds on
`todo/deep/adr0019-d2c-attribute-default-chunks.md` (the 2026-08-07 research pass) and a fresh
2026-08-08 survey.

## Position correlation: name-keyed is the answer, and now we know why positional fails

The D2b deferral reason ("position-correlating the precomputed vec with the registration-time
statement walk") is now precisely characterized:

- The runtime class walk flattens one level of `SyntheticBlock`, then
  `collect_nested_class_has_decls` **appends** nested-sub `has` decls to the **end**
  (`registration_class_body.rs:66-82,121`).
- The compiler-side collector `collect_attr_is_default_chunks` (`compiler/decl_plan.rs:139-186`)
  instead **interleaves** nested-sub attrs at the enclosing `SubDecl`'s position — a genuinely
  different order. (It also double-pushes a nested `has ... is default` — once from the
  `SubDecl` arm's direct loop, once from its recursion — harmless under name-keyed first-match
  lookup, but a latent trap any positional design would trip on.)
- Role walks have no nested-sub surfacing at all (`registration_role_decl.rs:194-231`), so
  positional would work there — but uniformity wins.

Since an attribute name is unique per class/role, **name-keying** (the `is_default_chunks`
precedent, `opcode.rs:2533-2538`, looked up in `registration_class_body_attr.rs:124-131`)
sidesteps order entirely. Decision: the D2b remainder lowers
`CompiledClassDeclPlan::attr_decls: Vec<(Symbol, CompiledAttrDecl)>` /
`CompiledRoleDeclPlan::attr_decls` **keyed by attribute name**, built by a collector that fixes
the double-push. `class_body_has_decl`/`role_body_has_decl` look up by the current
`Stmt::HasDecl`'s name and fall back to `CompiledAttrDecl::from_stmt` when absent (augment,
EVAL, runtime `has`, and any future mismatch) — the same guarded-degrade shape as every other
plan cutover. `is_default_chunks` folds into the lowered struct (its `is_default` field) and
the separate plan field retires.

## Compiled default/where chunks: the eval sites are ready; the work is construction-side

The 2026-08-08 survey confirmed all three env-setup shapes (A:
`attr_build_defaults.rs:287-335`; B: `methods_object_default_ctor.rs:204-298`; C:
`methods_object_dispatch_new.rs:2146-2163`) and both where-constraint sites
(`methods_object_attr_constraints.rs:6-22,356` — which set up **no** env at all) already funnel
through `eval_decl_trait_arg`, which handles `Compiled` transparently via `run_decl_expr`.
Chunks compile slot-free (`compile_decl_expr`, `decl_plan.rs:19-37`), so they read
`self`/`?CLASS`/`!attr`/`.attr` from whatever env the shapes populate — **no eval-site changes
are needed to flip the variant**. The construction-side work:

1. At plan lowering, compile each attribute's `default`/`where_constraint` expression into
   `DeclTraitArg::Compiled` inside the lowered `CompiledAttrDecl` (keeping the
   `compile_decl_trait_arg` literal short-circuit, so the 9 literal fast paths stay literal).
2. **Retire the two `as_expr` consumers that would panic on `Compiled`**
   (`DeclTraitArg::as_expr` is `unreachable!` on `Compiled`, `opcode.rs:2145-2153`):
   - `extract_shape_from_default` (`methods_object_dispatch_new.rs:1823`) reads the default
     `Expr` to derive a declared shape for `@`-sigil attrs — a **pure syntactic fact**;
     precompute it at lowering into a `CompiledAttrDecl::declared_shape` field (D2a pattern).
   - `.^attributes` introspection builds a build closure from the default `Expr`
     (`methods_classhow_attribute.rs:248-255`) — rebuild the closure around
     `eval_decl_trait_arg` on the stored `DeclTraitArg` instead of the raw `Expr`.
   These two must land in the same slice as (1) or the panic-free invariant documented on
   `ClassAttributeDef` (`runtime/mod.rs:448-454`) breaks.
3. The three role registry tables are already `DeclTraitArg`-valued (D2c-3); once role
   `attr_decls` carry `Compiled`, the tables receive `Compiled` for free (the earlier research
   pass confirmed role type params bind at env level, so a single compile-time chunk is sound
   for `is default(T)`-shaped role defaults).

The A/B env-setup unification (shape B's `has_class_scoped_subs` package-switch gate and extra
`__ANON_STATE__`/`constructing_class` bindings vs shape A's unconditional switch) remains a
**separate optional slice** gated on raku-behavior verification, exactly as the research pass
flagged — flipping the variant does not require it.

## Slices

- **D2b-2** — lower name-keyed `attr_decls` onto both plans (chunks still `Literal`/`Ast` at
  this point, i.e. a pure construction-site move: registration stops calling `from_stmt` for
  plan-backed walks); fold `is_default_chunks` in; fix the collector double-push. No behavior
  change; full `t/` + roast S12-attributes/S14-roles.
- **D2c-4** — compile `default`/`where_constraint` to `Compiled` inside the lowered structs +
  the two `as_expr` consumer retirements (+ `declared_shape` precompute). Measurable gain: the
  `Ast` variant's per-construction recompile disappears; verify with a
  construction-in-a-loop microbench and the same test set.
- **D2c-5 (optional)** — A/B env-setup unification after verifying shape B's gate against raku.

D2b-2 is also a **D6/D9 prerequisite**: the has-arm is one of the readers keeping
`legacy_body` alive (see `todo/deep/adr0019-d6-d9-legacy-body-removal.md`).
