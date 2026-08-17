use Test;

plan 2;

# `unit class Foo is repr<CStruct>;` had the same paren-only-trait-argument
# limitation as the block form (`t/is-repr-angle-bracket-trait.t`), plus its
# own separate hand-duplicated `is`/`does`/`hides` loop that discarded `repr`
# entirely regardless of syntax. A `unit class` statement extends to the end
# of the file, so this needs its own file.
#
# `.REPR` itself used to report P6opaque here regardless of angle/paren
# syntax: `unit class ...;` folds every trailing statement (including this
# file's own `is Foo.REPR, ...` check) into the class body, which runs at
# registration time — but the CStruct/CUnion/CPointer repr registration used
# to happen only *after* the whole body had already run, so a self-
# referential `.REPR` read observed nothing. Fixed by registering the repr
# before the body runs (`src/vm/vm_typedecl_ops.rs::exec_register_class_op`).

unit class Foo is repr<CStruct>;
has uint64 $.x;

is Foo.^name, 'Foo', 'unit class: is repr<CStruct> (angle form) still declares the type';
is Foo.REPR, 'CStruct', 'unit class: is repr<CStruct> (angle form) sets REPR, observable from within its own body';
