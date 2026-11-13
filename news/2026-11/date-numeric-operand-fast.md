# `Date == Date` no longer re-runs two full type checks per call

A numeric infix with a `Date` operand (`$start == $date`, `<`, `<=>`) cost
~18 µs, against ~1 µs on rakudo. Every call asked twice, through the full
uncached type check, whether each operand does `Real` (once for the `Bridge`
pairing, once for the numeric coercion). It then numified the Date through a
dynamic `.Numeric` method call.

The coercion now answers a plain `Date` with its day count directly, from the
same `temporal_core` routines as its `Numeric` row. The `Real` test answers an
instance of a built-in class from the type catalog, unless the program
declares the class or `Real` itself. `Date == Date` now takes ~2 µs.
Duration, Instant and user `Real` classes keep their bridging.
App::Moneymoor's period property test went from 44 s to 29 s (rakudo 6.7 s).
