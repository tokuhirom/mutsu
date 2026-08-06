# A labelled bare block / labelled `do` block is no longer treated as a loop

`LAB: { ... }` and `LAB: do { ... }` used to lower, at parse time, to a dummy
`Stmt::For` iterating a single `Nil` element — a leftover way of giving the
label somewhere to attach for `last`/`next`/`redo`. That made the block behave
like a real (if degenerate) loop construct: `last LAB` inside it just left the
block early, `next LAB` skipped to the end, and `redo LAB` re-ran it. rakudo
disagrees on all three — a bare block, labelled or not, is not a loop
construct, so `last`/`next`/`redo` naming it (or reaching it with no other loop
enclosing) is `X::ControlFlow` ("labeled last/next/redo without loop
construct"):

```
my $n = 0;
LAB: { $n++; last LAB; $n += 100 }
say $n;
```
```
raku : labeled last without loop construct
mutsu (before): 1
mutsu (after):  labeled last without loop construct
```

Both label sites in `src/parser/stmt/control/labeled_loop.rs` (`LAB: { ... }`
and `LAB: do { ... }`) now lower to a labelled `Expr::DoBlock` — the same node
plain expression-position `do { ... }` already uses — instead of the dummy
`Stmt::For`. That node's VM handler (`exec_do_block_expr_op`,
`src/vm/vm_misc_block.rs`) used to *also* catch a matching labelled
`last`/`next`/`redo` (added for a different reason — the general labelled-block
sink case), so that catching was removed there too: a `do`/bare block now only
ever intercepts `leave` for its own label; `last`/`next`/`redo` fall through to
`OpCode::Last`/`Next`/`Redo`'s existing "no loop-handler in scope" check
(`src/vm/vm_exec_dispatch.rs`), which now also renders the `labeled `-prefixed
message rakudo uses (`illegal => 'labeled last'` vs plain `'last'`) when the
signal carries a label.

Regression-tested against `t/loop-control-without-loop.t` (extended with four
new assertions for the labelled-bare-block and labelled-do-block cases,
verified against real `raku` first) plus the full `t/` suite, and
`roast/S04-blocks-and-statements/pointy.t` / `roast/S04-statements/do.t`
(both already exercise labelled blocks and stayed green).
