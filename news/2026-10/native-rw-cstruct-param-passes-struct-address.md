# `is rw` CStruct/CUnion native parameters pass the struct address

A NativeCall parameter declared `SockAddr-any is rw` (a `CStruct`, `CPPStruct` or `CUnion`)
was marshalled like `Pointer is rw`: C received a `void**` to a slot holding the address
instead of the struct's address, so `bind(2)` failed with `EFAULT`. Only a `Pointer` parameter
is an out-slot; an aggregate is already passed by its storage address. TCP::LowLevel's
`Linux Sockets` subtests (`t/01-basic-compat.t`) depend on this.
