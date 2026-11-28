# `for %h.values` with a read-only body stops promoting the hash's elements

`for %h.values { $sum += $_ }` used to promote every value of `%h` to a shared
container cell first, so that `$_` could alias the element (ADR-0045). It
then compared the topic with the element after every iteration to write it
back. Before the loop it also copied the hash's whole key order, for that
writeback.

A body that cannot write, bind or capture `$_`, call user code, or reach the
source by another name cannot tell a container from its value. That is the
`topic_alias_unobservable` analysis `for @a` already uses (#14495). For such
a body the compiler now emits the `.values` call as the by-value method call,
and the loop skips the topic writeback. A body that does write `$_`
(`$_ *= 10 for %h.values`) keeps both, and still writes through.

`benchmarks/hash-access.raku` (callgrind, JIT on, `BENCH_DET=1`) goes from
139.2M to 99.4M instructions (-29%). Its 10,000-iteration values loop goes
from about 6,470 to about 3,000 instructions per iteration.
