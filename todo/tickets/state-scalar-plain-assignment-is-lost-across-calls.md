# Type-constrained state scalars lose plain-`=` writes (residual)

UPDATE 2026-08-06 (2): the block-as-loop-body residual is FIXED — the
block-exit cleanup no longer treats a `state` slot as a block-local `my`
(its storage outlives the block; the loop's `sync_state_locals_in_range`
was writing Nil through the shared cell), and the #5959 sole-block
ResetStateLocals suppression is now gated on `Stmt::For`'s
`is_statement_modifier` so a nested bare block in a prefix `for` body
still re-clones per iteration (raku: 1 1 1). Pinned by
`t/state-block-loop-body.t`; details in
`news/2026-08/state-block-loop-body-accumulates.md`.

UPDATE 2026-08-06 (1): the original headline symptom — a `state` scalar
written with plain `=` in a routine losing the write between calls — was
fixed by StateVarInit's shared-cell storage (#5959), pinned by
`t/state-scalar-plain-assignment.t`.

## Residual: type-constrained state scalars

A TYPE-CONSTRAINED state scalar (`state Int $n = 0; $n = $n + 1`) still
loses the write (1,1 — raku: 1,2): it keeps the plain store, mirroring
`box_captured_lexicals`' rule that a constrained scalar must flow through
the assignment chokepoint so the constraint re-checks (and a
`state buf32 $w` holding a Buf must not present as a cell to the
element-assignment path — Digest's SHA2 `(state buf32 $w .= new)[$j] = …`,
pinned by t/digest-battery.t). Fixing typed scalars needs either
constraint-checking write-through on the cell, or the exit-persist rule
rework the original ticket described.

## Residual: nested-block state in non-`for` loops

`Stmt::While` (and the C-style/repeat arms) carry no
`is_statement_modifier` flag (44 construction sites), so their
sole-block suppression from #5959 still applies to a NESTED bare block:
`while $c { { state $n = 0; ... } }` persists state across iterations
where raku restarts it. The `for` twin is fixed (flag exists on
`Stmt::For`); fixing these needs the flag added to the other loop
statement variants or a parser-side marker on the body block.
