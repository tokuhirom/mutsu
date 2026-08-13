# `unit class Foo is repr('CStruct');` does not set `.REPR` at runtime

Discovered while adding angle-bracket (`is repr<...>`) trait-argument support
(the parser fix in `src/parser/stmt/class/{class_decl,role_decl,package_decl}.rs`,
`src/parser/helpers.rs::parse_trait_angle_arg`).

## Repro

```
$ cargo run -q --bin mutsu -- -e "unit class Foo is repr('CStruct'); has uint64 \$.x; say Foo.REPR"
P6opaque   # expected: CStruct
```

The block form works correctly:

```
$ cargo run -q --bin mutsu -- -e "class Foo is repr('CStruct') { has uint64 \$.x }; say Foo.REPR"
CStruct
```

This is **pre-existing** and independent of the angle-bracket-vs-paren
syntax — both `is repr('CStruct')` and `is repr<CStruct>` fail identically
for `unit class`, confirmed by AST dump: `--dump-ast` on the `unit class`
one-liner shows `ClassDecl { repr: Some("CStruct"), body: [.., HasDecl { .. }], is_unit: true, .. }`
— the parser and the AST-consolidation step that folds the rest of the file
into the unit class's `body` are both correct. The bug is downstream, in
either the compiler's plan-building (`compiler/decl_plan.rs::add_class_decl_plan`)
or the VM's `exec_register_class_op` (`vm/vm_typedecl_ops.rs`) not receiving
or not acting on `repr` for the `is_unit: true` path the way it does for the
block form — not yet isolated further.

## Why this is filed separately

Out of scope for the angle-bracket trait-parsing task this was found during:
that task's job is "the parser accepts and records `is repr<...>`/`is
ctype<...>`", not "make every existing `is repr(...)` combination register
correctly at runtime". Fixing this needs tracing `add_class_decl_plan` /
`exec_register_class_op` for the `is_unit` branch specifically, which is a
separate, self-contained investigation.

## Where to look first

- `src/compiler/decl_plan.rs::add_class_decl_plan` — confirm `repr` from the
  `Stmt::ClassDecl` actually reaches `CompiledClassDeclPlan.repr` for an
  `is_unit: true` declaration (vs. block form).
- `src/vm/vm_typedecl_ops.rs::exec_register_class_op` (around lines 104-301
  as of this writing) — confirm the `if let Some(repr_name) = repr { ... }`
  branch (CStruct/CUnion/CPointer registration) actually runs for a
  `unit class`-declared type; if it does run, check `register_cstruct_class`/
  `declared_class_repr` name-matching (`src/runtime/cstruct_layout.rs`) for a
  package-name qualification mismatch (`Foo` vs. a qualified `GLOBAL::Foo`
  or similar) between what gets inserted into `registry.cstruct_classes` and
  what `.REPR`'s lookup queries.

## Regression test once fixed

None yet — `t/unit-class-repr-angle-trait.t` only asserts `Foo.^name` (the
angle-bracket parsing this was found during, block-form covered by
`t/is-repr-angle-bracket-trait.t`) and explicitly notes this gap rather than
asserting the broken `.REPR` value. Once fixed, extend that test (or this
file) with `is Foo.REPR, 'CStruct', ...` for both the paren and angle forms.
