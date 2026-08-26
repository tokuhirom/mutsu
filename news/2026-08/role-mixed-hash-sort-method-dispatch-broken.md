# `.sort` on a role-mixed Hash sorts its pairs again

`%( 3 => 33, 4 => 44 ) but Lastable` answered `({3 => 33, 4 => 44})` from `.sort` — a one-element
list holding the whole unsorted hash — while `.keys`, `.elems`, `.map`, and `.grep` on the same
value all behaved correctly.

## Root cause

`sort_value_generic` (`src/runtime/methods_collection_ops/sort.rs`) dispatches on
`target.view()` and has arms for `Array`, `Seq`/`Slip`, the `Range` family, `Hash`, and
`Set`/`Bag`/`Mix`, ending in a correct catch-all: "any non-list value sorts as a one-element list
of itself" (`Any.sort` is `self.list.sort`). A `ValueView::Mixin` has no arm, so a role-mixed
Hash hit that catch-all and was treated as a scalar.

The interesting part is that `.map` and `.grep` were already right: their dispatchers call
`Interpreter::mixin_iteration_target`, a helper that already existed precisely to unwrap a role
mixin over a list-ish value for iteration. `.sort`'s dispatcher simply never called it.

## Fix

`dispatch_sort_method` (`src/runtime/methods_dispatch_match2.rs`) now applies
`mixin_iteration_target` to its target, exactly as the `map`/`grep` dispatchers do — one shared
mechanism rather than a second copy of the list-ish predicate. A mixin over a *non*-list value
(a punned role, `5 but R`) is left alone by that helper, so it still sorts as the one-element
list raku specifies.

Pinned by `t/role-mixin-survival.t`, which covers `.sort`/`.map`/`.grep`/`.keys`/`.elems` on a
role-mixed Hash and Array, including a role method that calls `self.sort` on its own invocant.
