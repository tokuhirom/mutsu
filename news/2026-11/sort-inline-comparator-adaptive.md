# An inline `.sort` comparator over Ints or Strs uses an adaptive stable sort

`@arr.sort({ $^b <=> $^a })` was already compared inline, without calling the
block, but it still ran mutsu's own top-down merge sort. Each merge level copied
both halves into fresh `Vec<Value>`s and cloned every element back, and each
comparison read the value out of the element's Scalar container again.

Now, when every element (after decontainerization) is a plain `Int` for a
`<=>` comparator, or a plain `Str` for `cmp`, the keys are extracted once and an
index permutation is sorted with the standard library's stable sort. The order
is a total order that no user code observes, so the result is the same as the
merge sort's. That sort is adaptive: an input made of a few ascending or strictly
descending runs sorts in linear time.

The general merge sort, still used for user comparators because it calls them
in a fixed (left, right) order, now sorts a permutation in one scratch buffer
instead of allocating two vectors per merge.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-array.raku` | 187,184,097 Ir, 39,537 allocations | 119,189,438 Ir, 19,582 allocations (-36%) |
| 10,000 ascending Ints, `{ $^b <=> $^a }` | 29,798,391 Ir | 12,696,324 Ir |
| 10,000 shuffled Ints, `{ $^a <=> $^b }` | 53,481,001 Ir | 35,045,378 Ir |
