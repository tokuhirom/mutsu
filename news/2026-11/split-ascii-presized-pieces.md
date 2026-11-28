# An ASCII split by a one-byte separator sizes its piece list up front

`$line.split(' ')` on ASCII text collected its pieces into a vector that grew
as the hits were found: a 20-word line regrew it five times. The ASCII path
(`split_ascii_by_string`) now counts a one-byte separator's hits in one
vectorized pass first and sizes the vector for every piece (bounded by a
limit when one is given).

callgrind, profiling build, JIT on, `BENCH_DET=1`
`benchmarks/word-count.raku`: 264,094,349 Ir → 255,269,895 Ir.
