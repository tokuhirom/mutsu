# Loop kernels read and store array elements and run C-style loops

A loop kernel (ADR-19288 D5) runs a whole loop over machine words. Until now
it declined any loop whose body subscripted an array, and it never saw C-style
`loop (...; ...; $i++)` statements at all. The index benchmarks' loops were
exactly that (`$asum = $asum + @arr[$i +& 15]`), so they paid the
interpreter's ~2,000-3,000 instructions per iteration.

Loop kernels now:

- read `@a[$i]` and store `@a[$i] = v` through campaign B's element helpers
  (#19294, `vm_kernel_elem`). A helper that finds anything other than a plain
  `Int`/`Num` element in a plain `Array` declines, and the iteration reruns
  generically. A loop never stores into an array it also reads, so an
  iteration replayed after a later bail rewrites the same elements;
- run C-style loops, as a `while` whose body is the body and the step;
- admit `++`/`--` on a carried `Int`, and `+&`, `+|`, `+^` and `%` on two
  `Int`s.

`benchmarks/bench-index-read.raku` (one run, `BENCH_DET=1`) went from 1,546.8M
to 655.7M instructions (-58%) and from 133.0 ms to 81.2 ms wall (rakupp v5.9.0:
33.9 ms; the hash loops are the rest). `benchmarks/bench-index-store.raku` went
from 1,179.7M to 669.9M (-43%) and from 112.2 ms to 61.2 ms (rakupp: 55.1 ms).
Both are kernels off against kernels on, on the same build.
