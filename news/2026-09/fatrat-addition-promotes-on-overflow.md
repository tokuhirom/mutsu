# FatRat addition promotes overflowing cross-products to BigInt

Adding two `FatRat` values whose intermediate numerator or denominator does not
fit in `i64` no longer panics the interpreter. The arithmetic now uses checked
`i64` operations and promotes the whole calculation to arbitrary-precision
integers when needed; subtraction follows the same path.

This fixes the golden-ratio sequence from the math documentation, where
`@phis[200].Str.chars` now returns `50` and the exact `FatRat` result is kept.

Pinned by `t/fatrat-add-overflow.t`.

Closes #7746.
