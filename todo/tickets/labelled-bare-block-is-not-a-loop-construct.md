# A labelled bare block handles `last`/`next` in mutsu, and does not in rakudo

```raku
my $n = 0;
LAB: { $n++; last LAB; $n += 100 }
say $n;
```

```
raku : labeled last without loop construct
mutsu: 1
```

mutsu's `exec_do_block_expr_op` (`src/vm/vm_misc_block.rs`) catches
`last`/`next`/`redo` whenever the block carries a matching label. rakudo does
not: a bare block is not a loop construct, labelled or otherwise, and only a
`for`/`while`/`until`/`loop`/`repeat` consumes the signal.

Found while writing `t/loop-control-without-loop.t` for
`news/2026-08/loop-control-without-a-loop.md`. It is *not* a regression from
that work — the labelled-block arm predates it — but that work is what makes the
divergence visible, because a labelled block now also raises the loop-handler
depth (`runtime/loop_handler_depth.rs`), so `LAB: { next }` reports nothing
where rakudo reports `labeled next without loop construct`.

## Why it is not a one-line deletion

Removing the arm changes what `LAB: { ... last LAB ... }` does from "leave the
block" to "raise `X::ControlFlow`", and mutsu's own `t/` suite may lean on the
lenient behaviour — check before deleting. rakudo's message is also more
specific than the generic one (`labeled last without loop construct`, i.e.
`illegal => 'labeled last'`), so the fix is a message variant as well as a
behaviour change. Do both together, and drop the `has_label` guard in
`vm_misc_block.rs` in the same commit so the depth stays honest.

## Related

`roast/S04-blocks-and-statements/pointy.t` and `roast/S04-statements/do.t` both
exercise labelled blocks and pass today; re-run them under `MUTSU_REAL_TEST=1`
before and after.
