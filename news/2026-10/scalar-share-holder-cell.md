# Scalar holders keep their own cell when sharing an aggregate

Assigning an Array or Hash to a scalar now gives the scalar its own container
around the shared aggregate. Writes through closures, `is rw` parameters and
topic aliases update the scalar without overwriting the source. Existing
bindings to the scalar see a new share, and chained scalar shares keep the
aggregate link when an earlier scalar is reassigned. Incrementing a scalar
that holds an Array or Hash reports the missing method instead of replacing
the scalar with `1`.
