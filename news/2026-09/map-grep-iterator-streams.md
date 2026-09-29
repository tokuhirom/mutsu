# `.iterator` over a `.map` / `.grep` Seq streams its source

`(1..5).map({ print "m$_ "; $_ }).iterator.pull-one` printed `m1 m2 m3 m4 m5`
before handing back `1`, and `@big.map(*+1).iterator.pull-one` ran the
callback over every element of `@big` (16 s for two million elements on a
debug build) — `.iterator` took the whole deferred Seq and wrapped the
materialized list (issue #10186, split out of the `for`-loop fix in #9936).

`.iterator` now steals the Seq's not-yet-run `MapGrep` source
(`SeqBody::take_map_grep_stream_source`) and keeps it on the `Iterator`
instance as a private stream. Every protocol method — `pull-one`,
`push-exactly`, `push-at-least`, `push-all`, `push-until-lazy`, `sink-all`,
`skip-one`, `skip-at-least`, `skip-at-least-pull-one`, `count-only`,
`bool-only` — pulls only the elements it needs from it
(`runtime/iterator_map_grep_stream.rs`), so the callback runs once per element
handed out, as Rakudo's map iterator does.

The instance's `items` attribute is a window of pulled-but-unconsumed elements
rather than the whole produced prefix: a top-up drops the consumed part before
appending, and a consumed window is emptied, so draining the iterator with
`pull-one` is O(n) overall rather than re-copying a growing prefix per call.
`Seq.new($it)` over such an iterator stays lazy, and the readers that take a
built-in iterator's remaining elements wholesale (`List.from-iterator($it)`, a
positional binding's coercion failover) drain the stream.
