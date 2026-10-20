# A `:=`-bound nested list is no longer treated as a shaped array

`my @a := [[1,2],[3,4]]; @a[5;0] = 1` died with "Index out of bounds" in mutsu,
while Rakudo grows the array to six rows. The bind path inferred a "declared
shape" from the bound value's rows and recorded it under a name-keyed
`__mutsu_shaped_array_dims::@a` env marker, which every multi-dimensional store
then honoured as a real shape.

A shape belongs to the array value (`ArrayData.shape`), and a bound shaped
array (`my @d := @s`, `my @e := Array.new(:shape(2,3))`) already carries it. The
marker, its `__mutsu_record_shaped_array_dims` builtin, the env latch that gated
it and the `MetaNs::ShapedArrayDims` namespace are gone — one more slice of
ADR-0097's retirement of name-derived binding metadata (#8087). The two element
fast paths that declined whenever any such marker might exist now take those
stores too.
