# `Seq.new($iterator).map`/`.grep` stream an iterator that does not say `is-lazy`

Rakudo's `map` and `grep` over a `Seq` are lazy whatever the source: they pull
the iterator only as far as a consumer asks. mutsu streamed the iterator only
when it advertised `is-lazy`, so `Seq.new($unbounded-iterator).map(*+1).head(3)`
(an iterator with the default `is-lazy`) pulled it to the end and hung. The
stage is now built over any not-yet-pulled iterator Seq, and the result is
marked lazy only when the source was, so `.is-lazy`, `.elems` and the strict
forces of a finite iterator are unchanged (#14271).
