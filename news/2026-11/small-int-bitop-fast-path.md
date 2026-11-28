# `+&`, `+|` and `+^` on two small Ints skip the operand dispatch

`$i +& 15` outside a loop kernel ran `pop_int_bitop_operands` first. That
popped both operands, asked for a user `infix:<+&>`, checked each operand for a
Failure, checked each for the numeric bridge, and then called the shared
`int_bitop`. `int_bitop` in turn ran its Str-operand check and its `Int`
coercions before the `&`. On bench-threads-serial's JIT-run write loop that
came to ~555 instructions per `+&`.

Now, when both operands are plain small `Int`s and no user infix of that name
is in scope, the op reads them where they sit and calls `int_bitop` directly.
`int_bitop` itself answers two small `Int`s before its coercions. It is still
the one implementation of the operators. A big `Int`, a `Str`, a `Rat`, a
Failure or a user infix all take the unchanged path.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-threads-serial.raku` | 179,455,109 Ir | 165,293,702 Ir (-7.9%) |
| `benchmarks/bench-index-store.raku` | 410,553,532 Ir | 386,248,617 Ir (-5.9%) |
