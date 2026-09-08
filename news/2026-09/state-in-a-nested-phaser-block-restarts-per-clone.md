# `state` in a nested phaser-carrying block restarts per clone again

A nested source `{ ... }` block is a block literal its enclosing block re-clones
on every execution, so its own `state` restarts each time. mutsu models that with
an `OpCode::ResetStateLocals` bracket. A block carrying an ENTER/LEAVE/KEEP/UNDO
phaser has to run through a real `BlockScope` instead of being inlined (inlining
would silently drop the phasers), and that route emitted no bracket — so in
**value** position the block kept its `state` across iterations:

```raku
my @r = do for ^2 { { LEAVE { }; state $q = 0; $q++ } };
say @r;
# raku:            [0 0]
# mutsu (before):  [0 1]
```

The phaser-free control next to it (`do for ^2 { { state $q = 0; $q++ } }`) was
always correct, which isolated the phaser branch as the cause.

## Narrowed further than the ticket assumed

Only the value-position tail diverged. The statement-position spelling
(`for ^2 { { LEAVE { }; state $q = 0; ... } }`) was already right, because
`compile_block_stmt` emits its `ResetStateLocals` *before* dispatching on the
block's shape, so the `PhaserScope` shape inherits it. A phaser block in a
routine tail was right for the same reason. The gap was confined to the two
value-position tail arms whose phaser-free sibling is `compile_bare_block_inline`
— the value-collecting `do for` body (`helpers_control_flow.rs`) and
`compile_block_inline`'s own tail arm.

## The fix

`compile_phaser_block_literal_inline` is `compile_bare_block_inline`'s twin for a
tail block that carries a phaser: the same `ResetStateLocals` bracket around
`compile_phaser_block_scope` instead of around the inline compile (the phaser
path's `BlockScope` already supplies the frame, so it adds no
`PushBlockFrame`/`PopBlockFrame`).

The bracket deliberately lives at the call site rather than inside
`compile_phaser_block_scope`, which is shared with `if`/`unless` branches: a
postfix statement modifier introduces no block at all, so its `state` must *not*
restart — the distinction `emit_branch_state_reset` exists to make. Bracketing
that function unconditionally would have broken those callers.

## Pins

`t/state-nested-phaser-block-clone.t` covers the value and statement positions
for both LEAVE and ENTER, a phaser block in a routine tail, and the two
deliberate *persist* cases that must not gain a reset — a loop body's own
non-nested `state`, and a postfix statement modifier. All eight assertions were
checked against rakudo.

`t/state-var-per-block-clone.t` and `t/state-nested-block-value-position-for.t`
(including their sole-block-loop-body persist cases) still pass unchanged.

Closes [#7632](https://github.com/tokuhirom/mutsu/issues/7632).
