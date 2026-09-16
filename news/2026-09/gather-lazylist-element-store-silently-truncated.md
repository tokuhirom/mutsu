# A bare `gather`/lazy-list element store silently truncated the source

`my $s = (gather { take 1; take 2 }); $s[0] = 5; say $s;` silently succeeded
and printed `[5]`, dropping element 2 entirely, where rakudo refuses the
store naming the touched element (`X::Assignment::RO`, "Cannot modify an
immutable Int (1)"). The same gap affected any bare (non-`@`-array-context)
`LazyList` — a `gather`, a finite `.lazy` view, an infinite sequence held in
a `$`-scalar, and so on.

## Root cause

This was the last open row of #7556's immutable-lvalue survey (section D).
The generic named-element-assign machinery
(`exec_index_assign_expr_named_op_inner`) had no arm at all for a
`ValueView::LazyList` target: its final fallback — meant to autovivify a
brand-new container into a `Nil`/undefined slot — has no way to tell "this
variable holds nothing yet" from "this variable holds a `LazyList`", so it
silently replaced the whole `LazyList` with a fresh one-element `Array`
built from just the touched index.

The array-context case (`my @a = 1,2,4...Inf; @a[2] = 99`, which really
does write through and must keep working) is already handled correctly,
earlier: `reify_lazy_array_slot` materializes the touched prefix into a
real `Array` before this op runs at all, gated on
`LazyList::in_array_context()`. That flag is exactly the
`array_context`/`list_context` oracle the ticket's own diagnosis said this
row needed — it already existed, just not consulted on this path.

## Fix

Added `try_lazylist_element_cell_assign`
(`src/vm/vm_var_assign_element.rs`), the `LazyList` twin of the existing
`try_seq_element_cell_assign` (which already does the equivalent job for a
bare `Seq`): for a positional store into a `LazyList` with
`!in_array_context()`, it force-reifies only up to the touched index
(bounded, so an infinite source stays live) and decides per element —
writes through a `ContainerRef` cell (the `take-rw @spot[1]`-style case),
or refuses naming the plain value the element actually holds (matching
`Seq`'s and rakudo's own per-element rule). A `Scalar`-wrapped element
(itemized at capture time) declines instead of guessing, for the reason
below.

## Found in passing: `take-rw` of a non-variable lvalue loses its container

The new check regressed `roast/S04-statements/gather.t`'s `take-rw`
subtest (old-issue-tracker #4668): `gather { take-rw my $ = 1 }` should
capture a genuine container, but pulled through this lazy (coroutine)
path the element arrives as `ValueView::Scalar(Int(1))`, not a real
`ContainerRef` — `take-rw`'s dedicated container-preserving compile path
only exists for a bare-variable operand (`Expr::Var`); anything else (an
inline declaration, here) falls back to a general compile that does not
reliably produce one through this path (the *eager* `gather` form does
preserve it correctly, so the loss is specific to the coroutine
item-collection/caching step). Since a plain `take` of an
explicitly-itemized value (`take $(1,2,3)`) arrives as the identical
`Scalar(value)` shape and rakudo genuinely refuses THAT one, the two
cases are indistinguishable at this point without deeper compiler
support. The fix declines on any `Scalar`-wrapped element rather than
guessing wrong in either direction, falling through to the pre-existing
(also imperfect, but not newly regressed) autoviv-replace behavior for
that narrower case. Filed as
[#8521](https://github.com/tokuhirom/mutsu/issues/8521).

## Found in passing: a separate, pre-existing `.eager` bug

Verifying the fix surfaced an unrelated, pre-existing gap: `.eager` (and
every other strict/eager force) short-circuits on ANY non-empty
`LazyList` cache, without checking whether the gather coroutine that
produced it has actually finished. A prior *bounded* pull (a plain index
read like `$s[0]`, or this fix's own reify-and-refuse probe) leaves the
cache holding only a prefix with the coroutine suspended, and the strict
path then answers `.eager` with that truncated prefix instead of resuming
it (`my $s = gather { take 1; take 2 }; $s[0]; say $s.eager;` prints `(1)`
instead of `(1 2)`). Filed as
[#8512](https://github.com/tokuhirom/mutsu/issues/8512) rather than folded
into this fix — it reproduces with a plain read and no assignment at all,
so it is independent of the element-store bug above, and touches a widely
shared force path that deserves its own measurement.

Pinned in `t/collections/lazy-seq/lazylist-element-store-refuses-defined-value.t`.
