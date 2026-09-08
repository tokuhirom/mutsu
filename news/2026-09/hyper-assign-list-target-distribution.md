# A hyper assignment into a list of lvalues now distributes its RHS

`($x, $y) »=» 5` died with `Index out of range. Is: 1, should be in 0..0`
where raku broadcasts the `5` into both targets. Every *other* hyper target
already broadcast a scalar RHS — `@a »=» 7` and `%h »=» 7` fill every slot —
so a literal list of lvalues was the one shape that could not take one.

## Root cause

`lower_hyper_assign_target` lowered a literal list of lvalues by *positional
index into the RHS*: element `i` of the target was assigned
`Expr::Index { target: source, index: i }`. That is a second, much weaker
implementation of the hyper distribution rule, and it got three things wrong
at once:

- a scalar RHS became `5[1]`, an out-of-range index rather than a broadcast;
- a short RHS padded with `Any` (`($x, $y, $z) »=» (5, 6)` gave `(5, 6, Any)`)
  instead of cycling to raku's `(5, 6, 5)`;
- a length mismatch under a non-dwimmy right arrow truncated silently
  (`($x, $y) »=« (5, 6, 7)` gave `(5, 6)`) instead of raising
  `X::HyperOp::NonDWIM`.

The element-wise lowering itself is not the problem — it is what makes nested
sublists work (`(($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7)`, pinned by
`t/hyper-assign-nested-destructuring.t`). Only its source of elements was.

## The fix

The RHS is now distributed across the target's shape *before* the
destructuring walk, by the ordinary hyper machinery: the lowering binds a
shape temp holding the target read back, then a second temp holding
`HyperOp { op: "=", left: shape, right: value }` with the same dwim arrows.
`»=»`'s leaf op yields its right operand, so that hyper op is exactly the
distribution and nothing else — the dwim rules, the cycling, and the
`X::HyperOp::NonDWIM` error all come from the one shared implementation in
`hyper_op_pair`. The destructuring below it is then a plain positional walk
over an already correctly-sized list.

Reading the target back is what supplies the shape, and it is what makes a
listy target element behave: raku fills `@a` in `(@a, $x) »=» 5` with one `5`
per existing element, which only a runtime read of `@a` can know. The one leaf
that cannot be read is a target element that *declares* its variable
(`(my $a, my $b) »=» (1, 2)`); it stands in as a scalar placeholder, which is
what a freshly declared scalar contributes anyway.

The RHS still evaluates exactly once — it is the distribution's right operand,
bound to a temp before any target is touched.

## Also fixed

The assignment's own value was the last element stored, not the distributed
list: `my $r = (($x, $y) »=» (5, 6))` yielded `6` where raku yields `$(5, 6)`.
The distributed list is now a named temp, so yielding it is what the lowered
block ends with.

`t/hyper-assign-list-target-distribution.t` pins all of it, and passes under
rakudo as well as mutsu.

Closes [#7586](https://github.com/tokuhirom/mutsu/issues/7586).
