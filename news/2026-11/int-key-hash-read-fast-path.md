# `%h{$int}` reads a plain hash without building a key String

The subscript read's associative fast path took only a `Str` key. An `Int` key, as in
`%h{$j +& 15}`, fell into the general subscript path: the allomorph, mixin, itemized-list and
lazy-slip probes, then a heap `String` built from the integer for the lookup, and freed again,
on every read.

A plain hash (no object-hash key type) files an `Int` under its decimal `Str`, which is a pure
function of the integer. The fast path now accepts an `Int` key, formats its decimal text into a
stack buffer with the same `Display` the general path uses, and looks the entry up by that text.
Object hashes, misses, `BigInt` keys and bound entries keep the general path.

Measured with callgrind on a 100,000-iteration loop reading `%h{$j +& 15}` (#19289): ~1,400
fewer instructions per read (marginal cost ~2,600 → ~1,200), and 0 allocations per read instead
of 1.
