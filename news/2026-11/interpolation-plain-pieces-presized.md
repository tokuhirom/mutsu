# A plain `"key-$i"` interpolation allocates its text once

The interpolation op drained its pieces off the stack into a fresh vector,
then appended them to an empty `String` that grew as each piece arrived. A
`"key-$i"` therefore cost about three allocations before the result's own
`Str` body: the vector, the buffer, and the buffer's regrowth.

When every piece is a plain `Str` or a plain `Int` (the case the op already
fast-pathed one piece at a time), it now joins them where they sit on the
stack, into a buffer sized up front for the whole result: the `Str` lengths
plus 20 characters per `Int`. The general per-piece loop is unchanged for
every other shape, and the join still goes through the same `Joiner`, so NFC
across seams and big shared `Str` pieces behave as before.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-hash.raku` | 185,797,052 Ir, 158,083 allocations | 177,607,360 Ir, 133,079 allocations |
| `benchmarks/hash-access.raku` | 97,376,254 Ir, 60,990 allocations | 94,111,767 Ir, 50,987 allocations |
| 20,000 × `my $s = "key-$i"` | 133,287,747 Ir, 93,523 allocations | 125,279,045 Ir, 63,522 allocations |
