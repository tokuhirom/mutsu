# Routines with `($i = ...)` or `my $x := ...` get a JIT body

The JIT's Tier A accepts a routine only when every opcode in it has a native
form or runs through the generic step shim. The expression-form local
assignment `AssignExprLocal` (every counter update inside an `nqp::while`,
`($i = nqp::sub_i($i, 1))`) and the `MarkScalarBindContext` marker of
`my $x := ...` were in neither table, so any routine containing one stayed on
the interpreter for good. Both opcodes are straight-line (a store or a flag,
then the next op) and are now step ops.

Array::Sorted::Util's `finds`, `inserts` and `insert-also` now run as native
bodies: Concurrent::PriorityQueue's single-thread `@a.push` loop does ~10%
fewer instructions per push (#15786).
