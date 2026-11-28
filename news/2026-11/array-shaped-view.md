# Rows of shaped arrays are `Array::ShapedView`s

A row of a multi-dimensional shaped array (`my @a[2;3]; @a[1]`) is now an
`Array::ShapedView`, the type rakudo 2026.11 introduced: a fixed-shape view of
the parent's storage. Its `.WHAT` is `(ShapedView)`, its `.raku` is
`Array.new(:shape(3,), [...])`, it reports the remaining dimensions as its
`.shape`, writes through it reach the parent and the parent's writes are seen
through it, and growing it dies with `X::IllegalOnFixedDimensionArray`.
`for @a[0] -> $x { ... }` now iterates the row's elements rather than binding
the whole row once, and `.Array` on a view copies it into a plain shaped
`Array` of the same shape.

Rows carry an `ArrayData.row_view` flag that the constructors of
multi-dimensional shaped arrays set on every row below the top. Storing a row
back into its own slot leaves the parent alone, so an element write through a
row does not replace the row with a copy. This closes the second half of
#19282 (jagged arrays shipped in #19314).
