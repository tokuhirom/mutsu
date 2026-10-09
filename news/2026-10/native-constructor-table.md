# The built-in constructors are one table

The 47-arm `match` on the class name in `dispatch_new_unallocated` and the duplicate branches of the VM's native construct path
now read one sorted table of native constructors (`src/runtime/native_ctor/`). They are not method-table rows: Rakudo declares
`new` on few of these types, the rest inherit `Mu.new` (ADR-11276 §9.49, part of #12423).
