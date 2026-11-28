# A short interpolation of plain pieces is written into one buffer directly

`"key-$i"` with plain `Str` and `Int` pieces went through `Joiner`, the
strand-aware joiner: a part list, a pending buffer, a flush and the strand
decision in `finish`. A result under `STRAND_MIN_BYTES` is built flat anyway,
so `join_plain_pieces` now writes such a result straight into one presized
buffer (the `Int`s through `IntDecimal`) and NFC-checks it exactly as
`finish` did. Longer results keep the joiner and its strands.

callgrind, profiling build, JIT on (on top of #19431):

| measurement | before | after |
|---|---:|---:|
| `benchmarks/hash-access.raku`, `BENCH_DET=1` | 51,304,218 Ir | 49,323,524 Ir |
| store loop `%h{"key-$_"} = $_ * 2`, per iteration | 3,065 Ir | 2,870 Ir |
