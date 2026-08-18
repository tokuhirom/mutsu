# `unit class Foo is repr('CStruct');` now sets `.REPR` at runtime

`unit class Foo is repr('CStruct'); has uint64 $.x; say Foo.REPR` used to
print `P6opaque` instead of `CStruct`, even though the parser correctly
recorded `repr: Some("CStruct")` on the `ClassDecl` AST node (confirmed via
`--dump-ast`). The block form (`class Foo is repr('CStruct') { ... }; say
Foo.REPR;`) always worked correctly.

## Root cause

`unit class Foo;` (the semicolon form) absorbs every trailing statement in
the compilation unit — including a `say Foo.REPR` — into the class's own
`body` (`parser::stmt::stmtlist`'s mainline-capture). That body runs at
*registration* time, inside `register_class_decl`.

`exec_register_class_op` (`src/vm/vm_typedecl_ops.rs`) used to register the
CStruct/CUnion/CPointer repr (`registry.cstruct_classes` etc.) only *after*
`register_class_decl` returned — i.e. after the class body, including any
self-referential `.REPR` read inside it, had already executed. For the block
form this was fine, since `say Foo.REPR` there is a separate mainline
statement that only runs after the whole `RegisterClass` op (repr included)
completes. For the unit form, the read happened too early and always saw the
default `P6opaque`.

## Fix

Moved the repr registration in `exec_register_class_op` to before
`register_class_decl` is called (right after `storage_name` is resolved),
so it happens before the class body — and any `.REPR` read inside it —
runs. `register_cstruct_class`/`register_cunion_class`/`register_cpointer_class`
are trivial `HashSet` inserts keyed by name, independent of attribute layout,
so moving them earlier is safe.

## Tests

- `t/unit-class-repr-angle-trait.t` — extended with `is Foo.REPR, 'CStruct', ...`
  (angle form: `is repr<CStruct>`).
- `t/unit-class-repr-paren-trait.t` (new) — same assertion for the paren form
  (`is repr('CStruct')`), the ticket's original repro.

PR [#6600](https://github.com/tokuhirom/mutsu/pull/6600).
