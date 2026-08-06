# `state` in a block used as a loop body loses every write (residual)

UPDATE 2026-08-06: the original headline symptom — a `state` scalar written
with plain `=` in a routine losing the write between calls — is FIXED:
`state` scalars now live in a `ContainerRef` cell exactly like `state`
aggregates already did (`StateVarInit`'s scalar branch; the "box-on-capture
will share them" assumption only covered captured scalars). Slot, env and
the state store share one cell, so the write-through reaches every reader
and the exit persist stores the same cell. Pinned by
`t/state-scalar-plain-assignment.t` (plain `=`, `+=`, `~=`, implicit/
explicit return, plain read after write, `++`, and the per-clone nested
named sub semantics).

## Residual: type-constrained state scalars

A TYPE-CONSTRAINED state scalar (`state Int $n = 0; $n = $n + 1`) still
loses the write (1,1 — raku: 1,2): it keeps the plain store, mirroring
`box_captured_lexicals`' rule that a constrained scalar must flow through
the assignment chokepoint so the constraint re-checks (and a
`state buf32 $w` holding a Buf must not present as a cell to the
element-assignment path — Digest's SHA2 `(state buf32 $w .= new)[$j] = …`,
pinned by t/digest-battery.t). Fixing typed scalars needs either
constraint-checking write-through on the cell, or the exit-persist rule
rework the original ticket described. (The expression-position typed state
decl also failed to register its constraint at all — fixed: the
`(state buf32 $w …)` expr branch now emits `SetVarType` like the statement
form.)

## Residual: the block-as-loop-body form

```
$ raku  -e '{ state $n = 0; $n = $n + 1; say $n; } for 1..3;'   # 1 2 3
$ mutsu -e '{ state $n = 0; $n = $n + 1; say $n; } for 1..3;'   # 1 1 1
$ mutsu -e '{ state $n = 0; $n++;        say $n; } for 1..3;'   # 1 1 1 (also wrong)
```

Two distinct causes, one fixed:

1. **(fixed)** the statement-modifier form parses as `For { body: [Block(...)] }`
   and the `Stmt::Block` arm emitted a per-execution `ResetStateLocals` INSIDE
   the loop body — the state store was wiped every iteration. The loop compile
   sites now set `Compiler::suppress_loop_block_state_reset` when the loop body
   is a sole source block (`loop_body_is_sole_block`), because that block IS
   the loop's body: cloned once per loop statement, its iterations share the
   clone (the loop-entry `reset_state_locals_in_range` still restarts the
   state when the loop STATEMENT re-executes).
2. **(open)** even without the reset, the write inside the block never reaches
   the state cell: gdb shows the per-iteration `StateVarInit` finds the stored
   cell still holding the initializer. Operator-independent (`=` and `++`
   both), so the block's scope machinery (BlockLocalScope env overlay) is
   shadowing the cell — the assignment writes a block-scoped plain copy
   instead of through the cell, and the block-exit scope restore discards it.
   The inline statement form (`for 1..3 { state $n = 0; ... }`, no Block
   wrapper) is correct, which isolates the defect to the Block-in-loop-body
   scope path.

Where to look: `exec_block_local_scope_op` (vm_exec_dispatch /
vm_misc_scope) — how a block-scoped env overlay treats a name whose outer
binding is a `ContainerRef` state cell; the write should go through the
cell, not insert a shadowing plain value.
