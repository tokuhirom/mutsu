# `.map` / `.grep` over a lazy iterator Seq pull on demand

`.map` and `.grep` used to fully reify a lazy iterator Seq before mapping or
filtering it. This covered `Seq.new($lazy-iterator)`, `Seq.from-loop`, and a
user iterator whose `is-lazy` returns True. On an infinite iterator, the
full reify never ended, so `Seq.new((1..*).iterator).map(*+1).head(2)` hung
(#10891).

Such a Seq is now a lazy pipe source:

- **Consuming the Seq.** The reify guard hands the untouched iterator to a
  fresh lazy Seq. The original Seq is still consumed, so a second `.map` on
  it throws `X::Seq::Consumed`, as in Rakudo.
- **Pulling elements.** `is_lazy_pipe_source` accepts the fresh Seq.
  `pull_source_element` reads it through the new
  `SeqBody::extend_from_iterator`, which calls the iterator's `pull-one`
  only as far as the stage needs.
- **Laziness of the result.** The pipe's finiteness check now treats such a
  Seq as unbounded. So `.is-lazy` is `True`, `.gist` is `(...)`, and `.elems`
  throws `X::Cannot::Lazy` instead of hanging.

A finite iterator Seq maps exactly as before.
