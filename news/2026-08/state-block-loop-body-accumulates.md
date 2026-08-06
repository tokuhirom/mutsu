# `state` in a statement-modifier block body accumulates across iterations

```raku
{ state $n = 0; $n = $n + 1; say $n; } for 1..3;   # raku: 1 2 3 — mutsu printed 1 1 1
```

Two defects stacked. First, the block-exit cleanup (`exec_block_scope_op` /
`exec_block_local_scope_op`) treated the `state` declaration as a block-local
`my`: it Nil'd the slot and dropped the env entry on exit, and the enclosing
loop's `sync_state_locals_in_range` then persisted that Nil THROUGH the
shared state cell (#5959's cell storage), so every iteration's
`StateVarInit` found Nil and the counter restarted. State slots (any slot
with a `StateVarInit` in the block's range) are now excluded from the exit
resets — a `state` variable's storage outlives the block by definition.

Second, fixing that exposed an over-broad #5959 suppression: the
sole-source-block ResetStateLocals skip applied to ANY loop body that is a
single block, which conflates the statement-modifier form (`{...} for @xs` —
the block IS the loop body, cloned once, state persists) with a nested bare
block in a prefix body (`for ^3 { { state ... } }` — re-cloned per
iteration, state restarts, raku prints 1 1 1). The suppression is now gated
on `Stmt::For`'s `is_statement_modifier`, so both shapes match raku. The
non-`for` loop variants lack the flag and keep the old behavior for the
nested corner — recorded as a residual in
`todo/tickets/state-scalar-plain-assignment-is-lost-across-calls.md`
alongside the type-constrained-scalar residual.

Pinned by `t/state-block-loop-body.t` (4 cases, verified against raku);
`t/state-var-per-block-clone.t` test 5 pins the nested re-clone.
