# `range_elems_f64` no longer collapses close BigInt-scale bounds to a wrong count

A `GenericRange`'s element count (used by `Range ~~ Numeric` and by the
WhateverCode range-index placeholder, e.g. `$range[*-1]`) was computed as:

```rust
let s = start.to_f64();
let e = end.to_f64();
let count = e - s + 1.0;
```

`f64` has a 52-bit mantissa, so once `start`/`end` are `BigInt`s whose
magnitude exceeds roughly `2**53` — two IPv6-scale addresses 10 apart, both
around `2**70`, are a real example — converting each endpoint to `f64`
independently rounds both to the *same* float. The subtraction then
collapses a genuinely non-zero difference to `0.0`:

```raku
my $start = 2**70;
my $end = $start + 10;
my $r = $start..$end;
say $r ~~ 11;   # raku: True   mutsu (before): False (elems computed as 1)
```

## The fix

`range_elems_f64`'s `GenericRange` arm now tries an exact `BigInt`
subtraction first — `end - start + 1`, adjusted for excluded ends — and only
converts the *result* to `f64`. The count itself is almost always small
enough to be exact even when the endpoints are not, so this fixes the
common case without changing the function's `f64` return type (still
consumed by three call sites unchanged: two `Range ~~ Numeric` smart-match
arms and the WhateverCode range-index bind). Endpoints that aren't a plain
`Int`/`BigInt` (a fractional `Num`/`Rat`, `Whatever`, `Inf`) keep the old
`f64`-based approximation, which was never the source of the precision
bug.

## Pins

`t/collections/range-pair/range-elems-bigint-precision.t` — inclusive,
excluded-start, excluded-end and excluded-both-ends BigInt-scale Ranges,
plus a plain small Range control, all checked against `raku`.

## Left open

The WhateverCode range-index consumer (`$range[*-1]`) also depends on
`range_params` (`src/vm/vm_var_index_ops.rs`) computing the actual indexed
*value* exactly for a BigInt-scale endpoint — a distinct bug tracked as
[#8588](https://github.com/tokuhirom/mutsu/issues/8588) and fixed by
[#8592](https://github.com/tokuhirom/mutsu/pull/8592). Before that lands,
`$range[*-1]` on a BigInt-scale `GenericRange` now computes the *correct*
placeholder value (thanks to this fix) but can still panic or answer
wrong when `range_params` saturates the endpoint to `i64::MAX` and then
overflows adding the correct offset to it; after #8592 lands, `range_params`
bails to its existing `value_to_list` fallback for an out-of-`i64`-range
endpoint instead, and the two fixes compose to answer the exact element.

Closes [#8591](https://github.com/tokuhirom/mutsu/issues/8591).
