# A nested bare block's `state` does not restart inside a value-position `for`

## Repro

```raku
my @r = do for ^3 { { state $x = 0; $x++ } };
say @r;
```

- `raku`: `[0 0 0]` — the inner `{ ... }` is a *nested* block literal that the
  loop body re-clones on every iteration, so its `state` restarts each time.
- `mutsu`: `[0 1 2]` — the state variable persists across iterations.

The statement-position form is correct:

```raku
for ^3 { { state $x = 0; say $x++ } }   # mutsu and raku both print 0 0 0
```

as is a value-position block outside a loop:

```raku
sub g { do { { state $x = 0; $x++ } } }
say g(); say g();                        # mutsu and raku both print 0 0
```

## What is (and is not) the cause

Not `Compiler::suppress_loop_block_state_reset`. The shared `for` lowering
(`src/compiler/control_for.rs`) now sets it from the statement-position rule in
both positions — `is_statement_modifier && loop_body_is_sole_block(...)`, which
is `false` for the prefix `do for ^3 { ... }` above — so the suppression is
already off and the nested block still does not reset. (Before the
statement/expression unification the value path suppressed it unconditionally
for a sole block, which masked this; removing that mask did not change the
output, which is how the second cause was found.)

So a `ResetStateLocals` is emitted for the inner block and does not take effect
on this path. The suspects are `compile_stmts_value`
(`src/compiler/helpers_control_flow.rs`, reached via
`compile_scope_restored_body_value`) and how it routes a trailing `Stmt::Block`
into `compile_do_block_expr_scoped`, versus where the collected-loop body's state
locals actually live.

## Affected files

- `src/compiler/helpers_control_flow.rs` — `compile_scope_restored_body_value`,
  `compile_stmts_value`.
- `src/compiler/helpers_do_expr.rs` — `compile_do_block_expr_scoped`,
  `emit_value_block_state_reset`.
- `src/compiler/helpers_stmt_analysis.rs` — `emit_nested_block_state_reset`,
  `patch_nested_block_state_reset`.
- `src/vm/` — the `OpCode::ResetStateLocals` handler and the slot range it
  clears.

## Why it is not a one-liner

`state` restart semantics are decided by *block cloning*, which mutsu models with
a compile-time `ResetStateLocals` bracket rather than by actually re-cloning the
block. Getting a nested block's bracket right inside a value-collecting loop body
means auditing which slots that opcode clears and whether the value path puts the
inner block's state locals in that range at all — and every neighbouring case
already passes (`t/state-var-per-block-clone.t`), so the fix has to be narrow
enough not to disturb them.
