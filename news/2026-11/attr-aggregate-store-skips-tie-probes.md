# Assigning a plain `@`/`%` attribute skips the tie probes

`@!r = ...` inside a method (bench-ctor's `TWEAK` does it once per object)
checks whether the attribute is a container class with its own `STORE` (`has
@.q is MyArray`) or a mixed-in container before taking the ordinary store.
When the attribute was a local slot of the method frame, those probes walked
the environment by name twice on every assignment, about 6.7k instructions,
even for the plain Array that is nearly every attribute.

The slot-based store now takes the same fast-out the by-name one already had:
when both the slot and the attribute's cell (the store of record) hold a plain
Array or Hash, there is nothing a probe could find, so they are skipped. A tied
attribute container still gets its `STORE`.

Callgrind on the profiling build: a class whose `TWEAK` does `@!r =
@!r.map(*.flat)` went from 86.8k to 81.9k instructions per construction, and
`benchmarks/bench-ctor.raku` (one run, `BENCH_DET=1`) from 453.9M to 443.9M.
Part of #19399.
