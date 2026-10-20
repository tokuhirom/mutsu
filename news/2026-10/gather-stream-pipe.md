# A streamed gather is a pipe over its Seq

The streaming consumers of a plain `gather` used to receive the gather's inner
coroutine `LazyList` itself. Those consumers are `Z`/`X`, triangle reductions,
the streaming builtins (`first`, `zip`, `roundrobin`, `flat`, `map`, `grep`)
and the `.map`/`.grep`/`.first`/`.lazy`/`.iterator`/`.flat` methods. Handing
over the inner list let it escape the Seq adapter (ADR-11678 invariant 1).

`gather_operand` is gone. Its replacement, `gather_stream`, moves the coroutine
of an uncached Seq into a fresh body that only the consumer can reach, which
consumes the original Seq as Rakudo's iterating operators do. It then hands out
a stream pipe (`LazyList::new_stream_pipe`) that pulls that body one element at
a time. A cached Seq is read through the pipe in place and stays readable. Each
pull appends only its new element (#14200), so streaming stays linear.

A stream pipe is exactly as lazy as its source, unlike an `Identity` index pipe,
which is `.lazy`. `gather { ... }.map(...)` therefore still answers
`.is-lazy` False and `.elems`. On the way, a `lazy`-marked pipe returned from a
map block (`(1,).map({ lazy gather take 5 })`) is no longer forced into a
reified Seq (#13026).
