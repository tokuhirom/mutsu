# A slice `:exists` on a shaped array no longer collapses to one Bool

`@a[0,1]` and `@a[0;1]` are different subscripts — a slice of two indices, and
one multidimensional index — and raku answers them differently:

```raku
my @z[3];
say (@z[0,1]:exists).raku;   # (Bool::False, Bool::False)
say (@z[0;1]:exists).raku;   # Bool::False
```

mutsu answered a single `Bool::False` for both, because
`exec_exists_index_adv_op` decided which subscript it was looking at by asking
whether the *target* was shaped:

```rust
ValueView::Array(items, ..) if crate::runtime::utils::is_shaped_array(&target) => {
    // Shaped array: multi-dimensional exists (e.g. @arr[0;0]:exists).
    let exists = Self::index_array_multidim(...);
    ...  // pushes ONE Bool and returns
}
```

So every comma slice on a shaped array was re-read as a multidimensional
subscript. `my @z[3]; @z[1] = 9; @z[0,1,2]:exists` came back `False` instead of
`(False, True, False)`, and it disagreed with the value adverbs on the very same
array — `@z[0,1,2]:v` correctly reported `(9,)`.

The shape of the target was never the right question, and it was never needed:
the separator the user wrote already survives to the compiler as two distinct
AST nodes, and `compile_exists` routes `Expr::MultiDimIndex` to
`__mutsu_multidim_exists_adverb` before `ExistsIndexAdv` is ever emitted. A
genuine `;` subscript could not reach that arm — it only ever fired for the
comma slices it then mis-read. Deleting it is the whole fix; an Array-valued
index is a slice, whatever the target.

`@m[0,1]:exists` on a 2-D `my @m[2;2]` now indexes the rows and answers
`(True, True)` like raku, while `@m[0;0]:exists` still addresses the one cell.

Pinned in `t/shaped-array-slice-exists.t`; all 16 assertions pass unmodified
under rakudo. Two neighbours found while pinning are recorded in
`todo/tickets/`: a Range index (`@a[0..1]:exists`) is read as a hash key rather
than a slice, and `:delete` on a shaped array discards the shape.
