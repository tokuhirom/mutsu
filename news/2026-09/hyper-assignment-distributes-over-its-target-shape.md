# A hyper assignment distributes over its target's shape

`news/2026-09/slice-assign-pads-short-rhs.md` made a plain slice assignment zip
its RHS against the slots and pad the tail. It kept the hyper spelling working
by marking it: `»=»` desugared to the same `IndexAssign` node as `=`, so a
`Stmt::MarkHyperSliceAssign` flag told the store to cycle the RHS instead of
padding it. That preserved the four slice rows it was measured against, but the
cycle-in-the-store was only ever an approximation of the hyper rule, and it was
already wrong wherever a slice store is not what the metaoperator lands on:

```raku
my @a = 1, 2, 3; @a »=» 7;       # raku [7, 7, 7]   was [7]
my @a = 1, 2, 3; @a[0..2] »=» 7; # raku [7, 7, 7]   was [7, Nil, Nil]
my @a = 1, 2, 3; @a[*] »=» 7;    # raku [7, 7, 7]   was [7, Any, Any]
```

The whole-container spelling stored a **one**-element array, because the parser
lowered `»=»` to a plain `target = value` and threw the dwim arrows away; nothing
distributed anything. The Range and Whatever subscripts reached a store the
marker did not cover.

## The fix

`»=»` is an assignment hyper-op like `»+=»`, so it now compiles as one. Its base
op is `=`, which yields its right operand at the leaf; the existing hyper
machinery supplies the distribution and the dwim-length rules, and the
compiler's assignment-hyper-op write-back stores the result. `is_assign_op` and
the base-op split in `compile_expr_hyper_op` grew the `=` case (the base op of
`»+=»` is `+`; the base op of `»=»` is `=` itself), and the base-op slice is now
computed only for an assignment hyper-op — every such op ends in the ASCII `=`,
where an arbitrary one may end in a multi-byte char and `&op[..op.len() - 1]`
panicked on `»×»`.

A literal list of lvalues (`(($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7)`) keeps
its own recursive lowering, which handles nesting the hyper machinery does not.

Because the distribution now happens in the hyper op, the list a slice store
receives is already exactly as long as the slice, and there is nothing left to
cycle. `Stmt::MarkHyperSliceAssign`, `OpCode::MarkHyperSliceAssign`, the
`hyper_slice_assign` one-shot `Cell` and `slice_rhs_value`'s `cycle` parameter
are therefore retired: one mechanism decides the rule, at the point that knows
it. All four of the marker's own hyper rows still pass, unchanged.

## Two more gaps closed with it

A hyper assignment has to know the slice's **arity**, and an associative slice of
an undefined scalar collapsed to a single `Any`: `my $h; $h<a b c>` answered
`Any` where raku answers `(Any, Any, Any)`. Its positional twin `$h[0,1,2]` was
already correct, so the associative arm of the `Package("Any")` subscript now has
the same shape.

Separately, a slice assignment's own **value** is the list it actually stored —
the RHS zipped against the slots, so both padded and truncated — not the raw RHS.
`(%h<a b c> = 1, 2)` is `(1, 2, Any)` and `(@a[0,1] = 1, 2, 3)` is `(1, 2)`. The
positional arm builds that list as it assigns; the associative arm, which exits
through the shared tail rather than returning early, hands it up in
`slice_assigned_rvalue`.

## Tests

`t/slice-assign-pads-short-rhs.t` grows from 20 to 30 assertions: the whole-array
and whole-hash hyper spellings, the Range and Whatever subscripts, the
undefined-scalar slice arity and a hyper assignment through it, and four rvalue
rows. Every one was run against `raku v2026.07` first.
`roast/S03-metaops/hyper.t`, `roast/S13-overloading/metaoperators.t` (which uses
`$a<a b c> »=» 42`), `t/hyper-assign-nested-destructuring.t`,
`t/index-assign-shadow-slot.t` and `t/unicode-ops-hyper.t` all stay green.

Two neighbourhood findings are filed rather than fixed here:
`todo/tickets/hyper-assign-to-a-list-of-lvalues-cannot-broadcast.md` (a scalar
RHS into `($x, $y) »=» 5` dies, because that lowering indexes the RHS
positionally) and
`todo/tickets/one-element-slice-assignment-rvalue-is-not-a-list.md`.
