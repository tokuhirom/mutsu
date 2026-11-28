# Jagged arrays: shapes with a `*` dimension

Rakudo 2026.11 gives a shaped array a dimension of no set length:
`my @a[2;*]`, `my @b[*;3]`, `Array.new(:shape(2, 3, *))`. mutsu now builds
and runs these jagged arrays the way rakudo does (#19282). Before, it ignored
the `*` and made a broken fixed array of empty rows.

A jagged array is an ordinary `Array` of rows that remembers its declared
shape (`ArrayData.shape`, with `0` standing for `*`). Each row is the array
the remaining dimensions describe:

- a growable `Array` for a lone `*`;
- a fixed shaped array (`my @b[*;3]` has rows of shape `(3)`);
- another jagged array (`my @c[2;3;*]` has rows of shape `(3 *)`).

What follows from that, matching rakudo:

- `.shape` reports the `*` (`(2 *)`), and `.raku` renders
  `Array.new(:shape(2, *), ([], []))`.
- A first dimension of set length has its rows made up front. It refuses
  `push`/`pop`/`shift`/... with `X::IllegalOnFixedDimensionArray`, and
  rejects an index past its length with rakudo's "Index 5 for dimension 1
  out of range" error. A `*` first dimension grows, making each new row of
  the row shape.
- `@a[1] = (1, 2, 3)` stores a row of the row shape, taking the items.
  `@a = ...` refills the rows and keeps the shape, while `my @x = @a` is a
  plain copy.
- `my Int @a[2;*]` checks each element written and fills the gaps with the
  type object. Two jagged arrays of one shape are `eqv`; a plain Array of
  Arrays is not.

The element-store fast lanes now leave a jagged array to the general store,
which makes the rows. The other half of #19282, the `Array::ShapedView` row
view of a fixed shaped array, remains.
