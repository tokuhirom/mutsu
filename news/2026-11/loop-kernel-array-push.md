# `@a.push($v)` runs inside a loop kernel

A counted loop whose body pushes onto an array never became a loop kernel,
because `ArrayPush` was not on the kernel's op list. `for 1..10000 -> $i {
@arr.push($i) }` therefore paid the interpreter's per-iteration cost plus the
lean push (#19385): about 2,400 instructions an iteration.

`ArrayPush` on a plain declared `@` local, with an `Int` or `Num` value, is now
a kernel element store. `carried_slots` records it as a store to the array.
The typing pass makes its result a value that only a `SinkPop` may consume. The
kernel calls `kernel_elem_push_int` / `_num`, which append under the lean
push's conditions:

- an ordinary `Array`, not a `List` or an itemized array;
- not a row view or a bound slice;
- no element type, default, shape or native backing;
- the entry's `:=`, readonly, constraint and alias checks;
- no second mutator thread.

Anything else declines and reruns the iteration generically, as before. That
covers a typed array, a read of the same array in the loop, a non-numeric
value, and a push whose result is used as a value.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-array.raku` | 94,262,691 Ir | 77,707,213 Ir (-17.6%) |
| 10,000 × `@arr.push($i)` in a sub | 27,190,302 Ir | 10,706,487 Ir |
