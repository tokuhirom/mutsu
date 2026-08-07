# ADR-0019 D2c-2: attribute defaults and `where` constraints run through `DeclTraitArg`

`ClassAttributeDef.default` and `.where_constraint` (`src/runtime/mod.rs`) were the last
two attribute-descriptor fields still typed as a raw `Expr`. This slice retypes both to
`Option<crate::opcode::DeclTraitArg>` — the same three-variant (`Literal`/`Compiled`/`Ast`)
enum ADR-0019 D2c-1 gave `is_default` — and migrates every reader to the shared
`Interpreter::eval_decl_trait_arg`/`DeclTraitArg::literal()` mechanism instead of each
site's own `Expr::Literal` pattern match plus a bespoke
`eval_block_value(&[Stmt::Expr(expr.clone())])` call.

A 2026-08-07 research pass (`todo/deep/adr0019-d2c-attribute-default-chunks.md`) had found
the real footprint of "compile defaults/constraints as child chunks" substantially larger
than the ADR text implied: not 3 near-duplicated env-setup blocks but at least 15 eval
sites across 5 distinct env-setup shapes (a "full" setup binding `self`/`?CLASS`/every
sibling attribute, a "full + extra" variant adding `__ANON_STATE__`/`constructing_class`
and a conditional package switch, a "minimal" self-only setup, several sites with no setup
at all, and the role-mixin `captured_env`-merge path). This slice covers every one of
them — `attr_build_defaults.rs`, `methods_object_default_ctor.rs`,
`methods_object_dispatch_new.rs` (both its pre-BUILD and per-class-attrs loops),
`methods_object_attr_constraints.rs` (`check_attribute_where_constraint`,
`construct_proxy_subclass`), `methods_dispatch_new.rs`'s native `bless` path,
`types/roles.rs`'s mixin-on-value path, `types/role_mixin_class.rs`'s runtime `does`
composer, `methods_classhow_attribute.rs`'s `.^attributes` introspection object, and
`registration_class_augment.rs`'s CUnion raw-bytes constructor — plus the six
`ClassAttributeDef` construction sites that feed them.

The scope is deliberately narrower than the ADR paragraph's "compile as child chunks"
framing: this is a mechanism *unification*, not yet a bytecode *precompilation*. Every
`DeclTraitArg` these sites build is still `Literal` or `Ast` — `CompiledAttrDecl.default`/
`.where_constraint` stay `Option<Expr>`, so a non-literal default's bytecode is still
compiled fresh on every construction, exactly as before. The near-duplicated env-setup
blocks were also intentionally left un-collapsed: the research pass flagged that the
"full + extra" shape's package-switch gate (`has_class_scoped_subs`) and its two extra
bindings looked like accidental drift from the "full" shape rather than a deliberate
difference, and merging them without first confirming that against raku behavior risked
silently dropping a binding one shape currently provides. Both are left for a later
slice.

Verified with the full `t/` suite (191 attribute/default/build/bless/role-related files,
1671 tests) and every roast-whitelisted `S12-attributes`/`S14-roles` file (33 files, 874
tests), all green with no output changes — this is a pure type/mechanism refactor.

Remaining under D2c: precompiling `default`/`where_constraint` chunks the way
`is_default_chunks` already does (the actual "child chunk" architecture and perf win),
and D2c-3 (the three `Expr`-valued role registry tables —
`role_attribute_default_exprs`/`role_class_level_attrs`/`class_attribute_default_exprs`
— which remain untouched since they are fed from `CompiledAttrDecl.is_default`/`.default`,
not `ClassAttributeDef`).
