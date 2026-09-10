use Test;

plan 2;

# Paren-form counterpart of t/unit-class-repr-angle-trait.t: `unit class Foo
# is repr('CStruct');` used to report P6opaque for `.REPR` even though the
# `repr` was correctly parsed and recorded on the ClassDecl AST node — the
# CStruct/CUnion/CPointer registration ran only *after* the class body (which
# a `unit class ...;` statement absorbs the rest of the compilation unit
# into) had already executed. Fixed by registering the repr before the body
# runs (`src/vm/vm_typedecl_ops.rs::exec_register_class_op`).

unit class Foo is repr('CStruct');
has uint64 $.x;

is Foo.^name, 'Foo', 'unit class: is repr(\'CStruct\') (paren form) still declares the type';
is Foo.REPR, 'CStruct', 'unit class: is repr(\'CStruct\') (paren form) sets REPR, observable from within its own body';
