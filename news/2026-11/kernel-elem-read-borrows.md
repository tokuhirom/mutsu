# A loop kernel reads an array element in place

A loop kernel's `@a[$i]` read (`kernel_elem_read_int` / `_num`, and the
two-level `@grid[$i][$j]` read) did four things per access:

- cloned the array node's `Gc` out of the slot or its `:=` binding cell;
- cloned the element `Value`;
- viewed the clone;
- dropped both again.

On a celled array that was ~161 instructions per read, most of them refcount
traffic.

The reads now borrow. A new `with_plain_array_node` runs a closure on the
node, which is borrowed from the slot's word or from the locked binding cell.
The element is probed in place with `Value::as_int` / `as_num`. The checks and
the answers are unchanged: a hole, a container element, a big `Int`, or any
other value still declines and reruns the iteration on the generic path.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-index-read.raku` | 150,241,462 Ir | 130,862,864 Ir (-12.9%) |
| `benchmarks/bench-threads-serial.raku` | 179,449,616 Ir | 165,707,214 Ir (-7.7%) |
