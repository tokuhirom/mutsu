# `state` scalars live in a shared cell; plain `=` accumulates across calls

`sub f() { state $n = 0; $n = $n + 1; $n }` returned 1, 1, 1 — every call
saw the initializer again. The scalar lived in the plain state store while
the assignment wrote only the routine's local slot; the exit persist then
read the stale env copy and stored it back, so the write never survived the
call. Only shapes that happened to force a cell-direct read (`say "in:$n"`)
or mutate in place (`++`) accumulated, which is why the existing `state`
coverage — all `++`-based — missed it
(`todo/tickets/state-scalar-plain-assignment-is-lost-across-calls.md`,
found while measuring ADR-0019 C6d-1).

Fix: `StateVarInit` now stores an untyped `state` scalar in a
`ContainerRef` cell, exactly as `state @a` / `state %h` aggregates already
were (Track B slice 3) and as scalars already were under an active thread
context. Slot, env and the state store share one cell, so the assignment's
write-through reaches every reader and the exit persist stores the same
cell. The old "scalars are already shared via box-on-capture escape
analysis" argument only covered scalars some closure captures. Per-clone
semantics are preserved (the cell is keyed by the scoped state key, so a
nested named sub still re-initializes per enclosing call).

Two carve-outs, both documented as residuals in the ticket: a
type-constrained scalar keeps the plain store (the same rule as
`box_captured_lexicals` — mutations must flow through the assignment
chokepoint for the constraint re-check, and a `state buf32 $w` holding a
Buf must not present as a cell to element assignment — Digest's SHA2,
pinned by t/digest-battery.t), and the block-as-loop-body form
(`{ state $n = 0; ... } for 1..3`) still loses writes to a block-scope
shadowing defect that predates this change. The statement-modifier block's
spurious per-iteration `ResetStateLocals` IS fixed (the loop compile sites
set `suppress_loop_block_state_reset` when the loop body is a sole source
block), and the expression-position typed state declaration
(`(state buf32 $w .= new)`) now registers its type constraint like the
statement form.

Pinned by `t/state-scalar-plain-assignment.t` (plain `=`, `+=`, `~=`,
implicit/explicit return, plain read after write, `++`, per-clone nested
named sub) — expected values verified against raku.
