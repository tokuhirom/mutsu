# A CArray element store takes the straight-line lane again

`CArray[num64]`'s `ASSIGN-POS` is `nqp::bindpos_n(self, nqp::unbox_i($pos), nqp::unbox_n($value))`.
Since #16411 such a body runs without a method frame (`run_straight_line_method`). The lane's gate,
`CompiledCode::ignores_role_context`, allows only local reads and `nqp::` ops. The compiler later
began ending a non-rw routine's tail call with `DerefContainer`, which decontainerizes the
returned value, as rakudo does. The gate did not know that op. From then on every store fell back
to the full method frame: a scoped env, the class-body statics and the parameter binder. Nothing
failed, but Sort::Fast's 1M-element fill slowed down roughly fivefold.

`DerefContainer` reads only the stack top, so the gate now allows it. A new `MUTSU_VM_STATS` line,
`method-lanes: straight_line=N`, counts the calls the lane answers.
`tests/carray_store_takes_straight_line_lane.rs` pins it for a `num64` store and an `int32` store
made inside a sub.

Callgrind slope of `@c[$_] = @a[$_].Num` on a `CArray[num64]` (profiling build, N 500 -> 1500):
140.8k -> 46.6k Ir per element. On the profiling build the 1M-element fill takes 4.8 s, down from
24.0 s. Refs #16215.
