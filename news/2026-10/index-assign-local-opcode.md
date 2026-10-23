# `IndexAssignLocal`: a slot-resolved element store has its own opcode

A named element store (`@a[0, 1] = 7, 8`, `%h<a b> = 1, 2`, `@a[$i] ~= $s`)
whose target the compiler resolved to a local slot now compiles to
`IndexAssignLocal { slot, .. }`. `IndexAssignExprNamed` lost its optional
`target_slot` and is emitted only for a target the compiler has no slot for:
a dynamic, a free variable, a global. Both run the same store
(`exec_index_assign_store`). For a local the running code declares, that
store operates on the slot itself (ADR-14632).

This retires `IndexAssignExprNamed` for slot-resolved locals, the last
condition of #14632. With it, the element, compound, nested and push-family
stores into a declared local read their constraint from the slot, and none
of them resolves its container by name.
