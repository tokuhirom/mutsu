# A closure call no longer clones a celled capture to snapshot it

`call_compiled_closure` snapshots every free variable at entry so it can tell at
exit whether the body changed one. For a captured `@`/`%` that lives in a
`ContainerRef` cell (ADR-16595 slice 1) that meant cloning the `Gc` handle,
comparing two handles to the same cell, and dropping the clone, on every call.
The cell is the variable's identity and its content is read and written through
it, so the snapshot now records it by address and the exit test is a pointer
comparison.

`.map({ @x[$_] })` over a captured array (#16642): see the PR body for the
per-call slope.
