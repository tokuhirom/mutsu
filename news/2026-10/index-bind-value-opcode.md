# An indexed bind's value marker is built by an opcode, not a magic-named call

`@a[i] := v` hands the index-assign ops a marker (the bound value plus the names of its source
variables). It used to be produced by a call to a `__mutsu_bind_index_value` builtin that the
compiler emitted as an ordinary `Call`, dispatched by name through `CallFunc`, and recognised by
the VM in some thirty places by comparing a `Pair`'s key with the string.

`OpCode::IndexBindValue` now builds the marker, and the marker's tag lives in one constant in
`vm_var_assign_ops.rs` next to `bind_index_marker`, `is_bind_index_value` and
`split_bind_index_marker`; every index-assign handler asks those instead of matching the key.
Deleted: the builtin and its dispatch arm, the call-name exemption in `vm_call_helpers`, the
compiler's `index_bind_marker_call` and the per-handler `Pair` name matches.
