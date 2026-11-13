# A user iterator's `push-all` reifies its Seq; cheaper nqp list ops and bound-scalar reads

When rakudo turns a `Seq.new($iterator)` into a list (`.list`, `.elems`, `.reverse`, `.Array`,
`.sort`, `.first(:end)`), its `List.from-iterator` calls the iterator's `push-all` once with an
`IterationBuffer`. mutsu always pulled one element at a time. The values came out the same, but a
class that writes a fast `push-all` never had it called. ParaSeq's `BufferIterator.push-all` hands
the rest of its input to the source list's own `push-all`; without that call, every element cost
a user method call. mutsu now makes the same `push-all` call. A class that only has the
`Iterator` role's default `push-all` is still pulled one element at a time, which is what that
default does anyway. ParaSeq's degree-3 `.first(…, :end)` over 200,000 elements went from 1.25 s
to 8 ms (rakudo 22 ms). `t/07-first` went from 7.6 s to 0.15 s.

Three per-operation costs of nqp-style loops also dropped:

- `nqp::atpos`, `nqp::elems`, `nqp::push` and `nqp::shift` on an `IterationBuffer` call their own
  implementation directly. They used to build an operand vector, probe for a `ConcBlockingQueue`,
  run the REPR check and match the op name first. That cut ~2,500 instructions per op to ~1,000.
- Reading a bound scalar (`my $x := …`) or a scalar captured by a closure now takes the local-read
  fast path. Before, it walked the full probe chain on every read.
- Assigning an object to a scalar (`$s = $iteration-buffer`) no longer arms the array-sharing
  lane. That lane only ever applies to an `Array`/`Hash` value.

ParaSeq: `t/05-head` 1.6 s -> 0.17 s, `t/06-skip` 3.2 s -> 0.23 s, `t/44-snip` 1.7 s -> 0.15 s,
`t/46-toggle` 5.0 s -> 2.0 s, `t/01-basic` 7.5 s -> 5.0 s, `t/09-map` 30 s -> 25 s.
