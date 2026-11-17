# IterationBuffer's methods refuse a P6opaque subclass

A class declared `is IterationBuffer` without `is repr('VMArray')` is
`P6opaque` in Rakudo. It inherits `IterationBuffer`'s methods, but each is an
`nqp::` op on `self`, so `SubBuf.new.push(1)` dies with `This type (SubBuf)
does not support positional operations` (`elems` for `.elems`, `.append` and
`.Seq`). mutsu's `.new` / `.bless` and the native constructor gave every
subclass element storage, and the method rows used it.

Now only `IterationBuffer` and a `VMArray` class get the storage
(`Interpreter::seed_iteration_buffer_storage`, next to `create_instance`), the
owner probe still routes a P6opaque subclass instance to the
`IterationBuffer` rows, and each row refuses it with Rakudo's message
(#17930). This finishes what #17627 did for the `nqp::` ops.
