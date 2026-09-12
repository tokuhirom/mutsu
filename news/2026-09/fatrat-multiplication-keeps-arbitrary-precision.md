# FatRat multiplication keeps arbitrary precision

Multiplying `FatRat` values no longer overflows the machine-sized numerator and
denominator once their products exceed `i64`. The operation now takes the same
BigInt-backed rational path as other arbitrary-precision rational operations,
so it remains exact and preserves the `FatRat` type.

The regression is pinned by `t/types/fatrat-product-overflow.t`.

Closes [#8049](https://github.com/tokuhirom/mutsu/issues/8049).
