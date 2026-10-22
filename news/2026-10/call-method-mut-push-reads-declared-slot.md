# The generic push/append path reads the declaring slot's element type

`@a.push(...)` and friends that go through `CallMethodMut` /
`CallMethodDynamicMut` rather than `ArrayPush` wrapped native-int arguments by
asking for the receiver's element type by name. `exec_call_method_mut_op`
now takes the call site's `target_slot` and passes the declared slot (`StoreDecl`)
to `wrap_native_int_items`, so a declared receiver reads only its own record.
Part of #14632.
