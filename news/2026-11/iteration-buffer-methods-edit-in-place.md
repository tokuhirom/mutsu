# IterationBuffer methods edit the buffer in place

`IterationBuffer.push`, `elems`, `AT-POS`, `BIND-POS`, `unshift`, `append`,
`prepend` and `clear` each copied the whole buffer out of its attribute and, for
the mutators, stored a fresh array back. A loop of `$ib.push(...)` was therefore
quadratic, and Concurrent::PriorityQueue's `List` method, which pushes one
`Pair` per element onto a buffer, spent 24 s of `t/02-concurrent`'s 48 s on
32000 elements (rakudo: 0.01 s).

The methods now edit the buffer's storage in place through the same editor the
`nqp::` list ops use (`nqp_with_elems_mut`, `bind_elem`, `resolve_index`), so
push is amortized O(1) and `elems`/`AT-POS` are O(1). As in rakudo, a `List`
taken from the buffer shares its storage and sees later edits, and a negative
`AT-POS`/`BIND-POS` index counts from the end. `t/02-concurrent` went from 48 s
to 23 s (profiling build, 4-core box). `.Slip`/`.Seq` still copy (#17658).
