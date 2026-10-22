# A `supply` block body runs through the compiled-closure path

ADR-0055 slice 4, last instance-level exemption. `call_sub_value` ran a
`supply { ... }` body on the tree-walk carrier, which recompiled the body's AST
and handed it its emitter. The body's compiled code already carries everything
that recompile re-derived (`is_supply_block_body`, the emitter symbol), so it now
runs through `call_compiled_closure` like every other closure.

The carrier had also given the body, as a side effect of going through
`run_nested`, the panic-to-exception boundary: a Rust panic raised by the body
(overflow, index out of range) reached the consumer's QUIT as a catchable
"Internal error". `call_compiled_closure` has no such frame, so the body's
re-entry point (`run_supply_body`) now wraps the call in
`run_guarded_against_panic`, which shares its `catch_unwind` and frame-recovery
steps with `run_inner_guarded`.

`call_sub_value` no longer has any closure with `compiled_code` that it keeps on
the tree-walk path.
