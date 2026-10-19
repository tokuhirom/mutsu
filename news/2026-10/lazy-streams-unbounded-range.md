# Streaming methods are one table for lazy lists and unbounded ranges

The streaming adaptors (`skip`, `rotor`, `batch`, `unique`, `repeated`,
`squish`, `produce`, `flat`) and the index views (`pairs`, `antipairs`, `kv`,
`keys`) answer a receiver that may not end with a lazy stage over it. They
were matched by name at three method entries, each with its own test of which
receivers qualify. They are now *streams* in `method_table::lazy_stream`,
registered by the owner Rakudo declares each method on and found through the
receiver's MRO, for a lazy `Seq`/`List`/`Array` and an unbounded `Range`
(`1..*`, `^Inf`, `"a"..*`) alike (ADR-13891 §4). The lazy refusals of a
`LazyList` (its `elems`, `sum`, `pick`, ...) stay its own: a `Range` keeps
answering those itself.

On the way, `Range.sum` no longer overflows: `(1..*).sum` panicked in a
debug build, `(1..^*).sum` and `(^Inf).sum` summed a capped prefix, and a
range whose sum exceeds 64 bits wrapped. An unbounded range sums to `Inf`, and
an Int range's sum is computed in big integers.
