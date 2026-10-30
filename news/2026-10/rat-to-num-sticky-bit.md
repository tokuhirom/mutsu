# Rat.Num rounds the exact ratio once

`bigrat_to_f64` truncated its scaled quotient and then rounded the truncated value, so a ratio
sitting just past a rounding midpoint (`-29849362802126903/6250000000000000`, a numerator past
2**53) came out one ulp low. A remainder now leaves a sticky low bit on the quotient, so the
conversion rounds the exact ratio. Data::Transformers' `standardize` (Rat inputs divided by a
`Num` standard deviation) now matches rakudo to the last digit; its suite passes.
