# `Seq.new($iterator).head($n).iterator` answers the Iterator protocol

`.head(n)` on a Seq over a user iterator returns a lazy Seq over mutsu's internal `HeadIterator`.
Taking its `.iterator` and calling `push-all`/`push-exactly`/`skip-at-least`... died with `No such
method 'push-all' for invocant of type 'HeadIterator'` (ParaSeq's `.IterationBuffer`, so
`t/05-head.rakutest`). `HeadIterator` now has an `Iterator` dispatch shape like `FromLoopIterator`
and a protocol step that reuses `pull_iterator_prefix_into`, so it stays lazy and never pulls the
wrapped iterator past `n` elements. ParaSeq's `t/05-head.rakutest` now matches raku.
