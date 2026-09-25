# `Seq.new($iterator).head(n)` pulls only `n` values

Closes #9353.

`.head(n)` on a `Seq.new($iterator)` drained the whole iterator before taking the first `n`.
An iterator that never ends but does not claim `is-lazy` made it hang:

```raku
class Forever does Iterator { has $.i = 0; method pull-one { $!i++ } }
say Seq.new(Forever.new).head(3);   # rakudo: (0 1 2); mutsu: hung
```

This showed up after #9333. Once `nqp::eqaddr` recognized the `IterationEnd` sentinel,
`Iter::Able::Cycle`'s iterator was really infinite, as it is in rakudo. Its
`t/07-cycle.rakutest` then hung on `cycle([2, 3]).head(5)`.

Rakudo's `head` pulls `n` times from the Seq's iterator, whatever `is-lazy` says. mutsu already
did this for `Str.comb` / `.lines` / `.words` cursors and `IO::Handle.lines` reads. That path is
`take_seq_prefix` over `SeqBody::take_prefix_source` (ADR-0119). The fix adds a third source
there: `PrefixSource::Iterator`. `take_prefix_source` now also takes a `SeqSource::Iterator`,
and `take_seq_prefix` calls `pull-one` at most `n` times, stopping early on `IterationEnd`.
`.head`, `.head(n)` and `.first` with no matcher all use it. The Seq is consumed, as in rakudo,
and a Seq that was `.cache`d keeps going through the ordinary reify path.

`Seq.from-loop({ ... })` with no condition wraps its body in a synthetic `FromLoopIterator`.
That iterator has no `pull-one`. Its driver now takes the same limit, so
`Seq.from-loop({ 42 }).head(3)` answers `(42 42 42)` instead of hanging.

Pinned by `t/collections/lazy-seq/seq-new-iterator-head-prefix.t`, which also checks that
`head(3)` calls `pull-one` exactly three times.
