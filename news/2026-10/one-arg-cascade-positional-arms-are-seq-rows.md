# The 1-argument cascade's positional arms are `Seq`'s rows

A lazy or unreified `Seq`, a `LazyList` and a `gather` have no settled dispatch
shape, so their `.AT-POS($i)` and `.EXISTS-POS($i)` were answered by two arms of
the 1-argument native cascade, which called the same element readers as `Seq`'s
rows. The arms now hand the call to `Seq`'s rows (ADR-11276 slice 5), and the
`Match.replace-with` arm, which duplicated `Match`'s own row, is gone. The
1-argument cascade has no method arm left: what remains of it are the receiver
adapters (a `Scalar`, an itemized hash, a native-base subclass, an allomorph, a
`Backtrace`, a shapeless `Match`).
