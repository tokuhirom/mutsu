# A kept short-circuit assignment retains a parameter container

`($parameter //= value) += 1` now raises the read-only parameter error when
the short circuit keeps its current value. The same rule applies to `||=` and
`&&=`. A value-bound name still returns an immutable value, while a writable
scalar still returns its container for the outer assignment.
