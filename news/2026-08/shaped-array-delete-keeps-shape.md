# A shaped array survives `:delete`

A shaped array is fixed-size: `:delete` empties a slot, it cannot shorten the
array. mutsu shortened it — deleting the last assigned slot collapsed the whole
thing:

```raku
my @z[3];
@z[2] = 3;
@z[2]:delete;
say @z.raku;    # was: []   raku: Array.new(:shape(3,), [Any, Any, Any])
say @z.elems;   # was: 0    raku: 3
```

`trim_trailing_array_holes` pops every trailing hole off the backing after a
delete. That is *right* for an unshaped array — `my @a; @a[0] = 1;
@a[0,1]:delete` really does leave `[]` — so the trim was only ever wrong when
the array had a declared shape, which is why it survived: a delete that strands
no trailing hole (`my @w[3] = 1,2,3; @w[1]:delete`) never reached it. The trim
now returns early for `ArrayKind::Shaped`, which also keeps the embedded shape
metadata attached, so `.raku` still prints `Array.new(:shape(3,), …)`.

## An empty slot deletes to `Nil`

The other half of the same rule. Where an unshaped array answers the `Any` hole
for a slot that was never there, a shaped array answers `Nil` — the slot is in
range, so "nothing was here" cannot be reported by a shorter array:

```raku
my @y[3];
@y[0] = 1;
say (@y[0, 1]:delete);    # was: (1 Any)   raku: (1 Nil)
my @u[3];
say (@u[0..2]:delete);    # was: (Any Any Any)   raku: (Nil Nil Nil)
```

It is the *slot* that decides, not the value: an explicitly assigned `Any`
(`my @z[3]; @z[0] = Any`) is not a hole and still deletes to `Any`. The
predicate is `ArrayData::hole_at`, the same one `:exists` and `:k`/`:p` use, so
the three agree.

## The multi-dimensional path had its own trim

`multidim_delete` truncated trailing `Any`s independently of the trim above,
which made `my @a[2;2]; @a[0;1]:delete` produce a ragged
`[[], [Any, Any]]` — and a later `@a[0;1]` read then indexed past the end of the
now-empty row and panicked with a subtraction overflow in
`check_shaped_array_bounds`. Both of its truncation sites (the indexed arm and
the `*` arm) now skip shaped arrays.

## `.raku` of a typed shaped array

Found alongside: a shaped array with a *boxed* element type rendered as
`array[Int].new(…)`, the lowercase native-array name. Only a native element type
is `array[int]`; a boxed one is `Array[Int]`, which is what the delete tests
above assert on.

Pinned by `t/shaped-array-delete-keeps-shape.t` (36 assertions, all of which
also pass under `raku`).

One thing deliberately left alone: `:delete` on a *partially* dimensioned slice
of a shaped array (`@a[*;*]:delete`, `@a[0;*]:delete`) is `X::NYI` in raku under
both 6.d and 6.e. mutsu implements it; the change here only stops it from
destroying the array, and does not try to reproduce raku's Failure.
