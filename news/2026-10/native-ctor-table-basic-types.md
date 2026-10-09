# Basic-type constructors join the native constructor table

`Int`, `Num`, `Str`, the allomorphs (`IntStr`, `NumStr`, `RatStr`, `ComplexStr`), `ObjAt`,
`ValueObjAt`, `Capture` and `Failure` are now entries of the `native_ctor` table instead of
separate `.new` ladders in `methods_dispatch_new.rs`, the interpreter's basic-type `match` and the
VM's `try_native_builtin_construct`. The `Buf`/`Blob`/`utf8` entries are marked `fast`, so the VM
path reads them from the table too. ADR-11276 §9.50; part of #12423.
