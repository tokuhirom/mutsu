# A finite `.lazy` Seq is single-pass again

`(1..5).lazy`, `(1,2,3).lazy`, and the `lazy 1,2,3` prefix are stored
internally as a `LazyList` whose cache is pre-filled rather than as a
`Value::Seq`, so they fell outside the `X::Seq::Consumed` single-pass
tracking every other Seq shape (`.map`, `.grep`, a deferred
`IO::Handle.lines`, ...) already honours. A second `.eager` (or `.sink`, or
a `for` loop) on the same value silently re-answered instead of throwing,
while the equivalent `.map` Seq already threw correctly.

A new choke point, `claim_lazy_seq_touch`, claims this exact LazyList
shape's one materializing touch and throws `X::Seq::Consumed` on a second,
reusing the existing consumed-tracking identity set. It is called wherever
such a LazyList gets force-materialized: `.eager`'s direct dispatch, the
generic force-and-redispatch bridge shared by most other forcing methods,
and `for`'s raw-items fallback. Three shapes stay exempt, matching raku: a
genuinely infinite/lazy source (still pulling on demand) is guarded by
`X::Cannot::Lazy` instead; one assigned into an `@` array is that array's
own backing store, not Seq semantics; and an explicit `.cache` call grants
multi-pass reads, as it should.
