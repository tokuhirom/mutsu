# `does` on a value parameter reaches the caller's container

`$v does R` on a readonly parameter holding an `Int`, `Str` or another value type now updates
the variable, array element, hash element or attribute the argument came from, as Rakudo's
rebless of the shared object does (ADR-14144, #14144). PDF's `coerce` callbacks rely on it.

The parser marks a parameter its routine body applies `does` to with the internal `does-target`
trait. Only a marked parameter keeps its argument's source container and writes the composed
`Mixin` back at return, so no other call pays for it. The parameter stays readonly. A copy taken
before the call keeps the plain value, which Rakudo's shared box would also change; a test pins
that divergence.
