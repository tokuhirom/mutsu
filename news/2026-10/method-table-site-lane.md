# Built-in method calls on variables skip the dispatch probes

ADR-11276's slice 1b puts the built-in method table in front of
`CallMethodMut`, the opcode a method call on a variable compiles to. When the
receiver is a plain `List`, `Array`, `Hash`, `Str`, `Num` or `Rat` and the
method has a row in the table, the row answers the call before the opcode's
chain of probes runs. Those probes cover accessor lanes, user `find_method`,
Proxy and lazy-Seq handling, Failure explosion and the writeback around the
call, and for such a receiver every one of them declines.

Whether user code augmented the receiver's type with the method is the one
check that needs the registry. Each method-name constant in a chunk now
remembers its answer, together with the receiver shape and the row, for one
registry write generation. A repeated call is then a lock, two compares and
the handler.

Debug builds still run the full path after the lane and assert that both give
the same answer. The whole `t/` suite passes with that check on.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `@a.elems` | 1,264M | 369M | -70.7% |
| `%h.elems` | 1,258M | 367M | -70.8% |
| `$s.chars` | 658M | 369M | -43.8% |
| `$n.isNaN` | 531M | 277M | -47.8% |
| `$r.numerator` | 1,131M | 305M | -73.0% |
| `@a.map(*+1).elems` (no row) | 9,264M | 9,271M | +0.1% |
