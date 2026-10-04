# An element share is an operand of its store

`@aoa[i] = @row` shares the row by reference while still being an assignment: a later
`@aoa[i] = 42` replaces the slot instead of writing through the shared row. The store
learned it was such a share from a `MarkElementShare` opcode, which set
`Interpreter::element_share_pending` for the next instruction to read and clear. Three fast
lanes each had to check that flag so they would not commit a store that should be marked.

The compiler always emitted that marker immediately before the one `IndexAssignExprNamed` that
consumed it, so the fact is now that opcode's `element_share` operand. It is fixed when the
code is compiled, no field can be left pending, and the early lanes are simply skipped for a
share (ADR-10779 D3).

`rw_param_rebinds` is also refiled from `handoff` to `frame`. It is saved and restored with
each VM call frame, so it describes the running frame, not a value passed between caller and
callee.
