# Loop kernels read and store `%h{$i}` and read `@g[$i][$j]`

Loop kernels (ADR-19288 D5) could already subscript an array, but not a hash
or an array of arrays. Those two shapes were the rest of the index
benchmarks.

They now read and store `%h{$int}` and read `@g[$i][$j]` through campaign B's
helpers (`vm_kernel_hash`, #19372):

- a hash key is the decimal string of the `Int`, formatted on the stack;
- a grid's row must be a plain array held directly in the outer element;
- `+>` on two Ints is admitted, as an arithmetic shift by 0 to 63 bits (any
  other count bails).

As with arrays, a region that both reads and stores the same hash declines,
and so does a stored hash or array that is reachable through another name. A
region that reads a grid declines if it stores into any element at all, since
a row has no identity of its own to compare.

`benchmarks/bench-index-read.raku` (one run, `BENCH_DET=1`) went from 655.7M to
191.2M instructions and from 81.2 ms to 34.8 ms wall. rakupp v5.9.0 takes
33.1 ms. `benchmarks/bench-index-store.raku` went from 669.9M to 444.8M and
from 61.2 ms to 58.6 ms (rakupp: 47.7 ms). With kernels off, the same build
takes 1,545.4M and 1,183.5M instructions.
