# Iterator `push-*` methods append to an IterationBuffer target

`(1,2).iterator.push-all(IterationBuffer.new)` left the buffer empty (#9419):
the built-in iterators' `push-all` / `push-exactly` / `push-at-least` /
`push-until-lazy` appended only when the target was an Array, through
`iterator_append_to_array_arg`, and the squish iterator carried two private
copies of the same Array-only append. lizmat's `ForwardIterables` /
`ReverseIterables` build their state with
`@iterables.iterator.push-all(my $iterables := nqp::create(IterationBuffer))`,
so every iterator they produced was empty.

`iterator_append_to_array_arg` now appends to an `IterationBuffer` in its own
storage (through `nqp_with_elems_mut`, the path `nqp::push` on a buffer
takes), and the squish iterator calls it instead of its copies.

Found along the way and filed separately: a user class that `does Iterator`
gets none of the role's default methods (#9466).

Pin: `t/collections/lazy-seq/iterator-push-into-iterationbuffer.t`.
