# The container-capture edge is documented next to the rest of cell sharing

ADR-0032 generalized `WrapVarRef` container capture (a `key => $v`, `Pair.new`,
`\($v)` or list built inside a closure aliasing the outer `$v`'s container) from
"directly nested named sub only" to every nested code kind, but the mechanism
was only written down in the ADR and in comments on `CompiledCode` fields.
`docs/captured-outer-cell-sharing.md` now has a §11 that explains it where the
rest of the captured-outer cell campaign lives: the emit-time edge
(`emit_wrap_var_ref` → `container_ref_capture_syms`), the slot-addressed
bubbling to the declaring frame (`bubble_container_ref_capture_syms` →
`needs_cell_ref_capture_slots`), the upvalue-promotion exclusion that keeps the
read a by-name env-cell recovery, the two false-positive exclusions (rw-arg
call-argument tags, for-loop parameters / `my enum` syms), and how this
aliasing-driven detector relates to the write-driven ones. ADR-0032's
"Remaining" list no longer carries the open docs note (#9931).
