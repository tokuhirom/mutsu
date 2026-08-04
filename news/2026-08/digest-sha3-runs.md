# `Digest::SHA3` produces the right digest — four more general fixes

`sha3_256("abc")` now returns `3a985da7…31532`, matching rakudo. Getting there
took four independent interpreter bugs on top of the two already landed for this
distribution (`news/2026-08/multi-named-narrowness-declaration-order.md` and
`news/2026-08/samewith-inside-lazy-gather.md`). Each one is general; none is
specific to `Digest`. The remaining `todo/tickets/digest-dist-blockers.md` §6
entry is closed by this.

## A `given` statement modifier is not a placeholder scope

    sub ROL64 { ($^a +> (64 - $_) +| $a +< $_) % (1 +< 64) given $^n % 64 }

came out with the single parameter `$^n`: `collect_ph_stmt_shallow`'s
`Stmt::Given` arm collected placeholders from the topic only, which is right for
a `given {}` BLOCK (its body is its own scope, and a placeholder there is the
block's parameter bound to the topic) but wrong for the statement-modifier form,
which introduces no block at all. `Stmt::For` already made exactly this
distinction via `is_statement_modifier`; `Stmt::Given` now does too, and the
compiler's matching "bind the body placeholder to the topic" step is skipped for
the modifier form. `ROL64` was silently rotating by the wrong amount.

## A multi-dimensional subscript is a list-assignment target

    ($current, @lanes[$x;$y]) = @lanes[$x;$y], ROL64 $current, …

failed the compiler's list-assignment target gate (which admitted `Expr::Index`
but not `Expr::MultiDimIndex`) and fell through to the runtime's "cannot assign
through non-callable value". It is a single-item target, like a single-index
`@a[i]` one, and compiles to a `MultiDimIndexAssign` reading its item from the
same decontainerized RHS snapshot the other targets use — so a swap
(`(@s[0;0], @s[1;1]) = @s[1;1], @s[0;0]`) works.

## A sized buffer is `Buf[uint8]`, not `buf8`

`multi KeccakF1600(blob8 $state)` lost every dispatch to its sibling
`multi KeccakF1600(@lanes)`, so the permutation never ran and the digest came
out as the padded input. Two things hid the buffer from the type-distance table:
`.^name` renders a sized buffer parameterized (`Buf[uint8]`) while the source
spells it `buf8`, and `value_type_name` answers the generic `"Any"` for every
instance — so the Buf/Blob family table, keyed on the short spelling of
`value_type_name`'s answer, never matched one. Both spellings are recognized
now, the lookup falls back to the instance's class name, and a `bufN` correctly
ranks its same-width `blobN` as an ancestor (`buf8 ~~ blob8` is True, but
`blob8` was not in the `buf8` chain at all).

## A Range subscript is a slice on a Buf

    $new-state[$_ ..^ $_ + 8] = store64 @lanes[$i;$j];

reported "Index out of range". The Buf element-assign path handled a comma list
(`$b[0,1,2] = …`) and a single index, but a Range reached the scalar arm and
failed `index_to_usize`. A finite Range is now expanded to the index list the
slice arm already consumes; element-width masking is unchanged (`buf8[0..1] =
300, -1` still stores `44, 255`).

Pinned by `t/given-modifier-placeholder-scope.t`,
`t/sized-buffer-dispatch-narrowness.t` and
`t/buf-range-slice-and-multidim-list-assign.t` — 29 tests, all also passing
under `raku`.
