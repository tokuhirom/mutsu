# Rational and Complex `.Numeric` coercions keep their values

`.Numeric` now preserves exact `Rat`/`FatRat` values and keeps `Complex` values
complex. This fixes rational round-trips through `Numeric.Rat` and prevents a
non-zero imaginary component from being silently discarded.
