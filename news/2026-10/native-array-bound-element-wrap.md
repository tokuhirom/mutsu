# A write through a bound native array element wraps like a direct store

A direct store into a native integer array wraps (`my uint8 @u; @u[1] = 300` stores 44). Before
this change, a store through a bound element did not:

```raku
my uint8 @u = 1, 2;
my $q := @u[0];
$q = 257;
say @u;     # [1 2]; it was [257 2]
```

The same gap showed up in `nqp::atposref_*`, an `is rw` routine returning an element, and `++` /
`+=` through such an alias.

Every write into a `ContainerRef` cell now goes through one chokepoint,
`coerce_container_cell_store`. It used to be the check-only `check_container_cell_constraint`.
When the cell's constraint is native, it coerces the value with the same routine a native scalar
store uses: integer types wrap, a full-width `int` refuses an oversized bigint, and `num32` rounds
to single precision. It then type-checks the coerced value and returns it as the value to store.

Growing a native array past its end through a bound element (`my int16 @h; my $d := @h[3]`) now
fills the gap with the element type's zero (`0`, `0e0`, `""`) instead of its type object. The two
copies of that hole computation are now one, `ArrayData::hole_value`.

Fixes #11233. Two related gaps are filed separately:

- #11450: a `num32` direct element store or push keeps double precision.
- #11451: `nqp::atposref_i(@a, 0) = 5` dies with "Unknown call".
