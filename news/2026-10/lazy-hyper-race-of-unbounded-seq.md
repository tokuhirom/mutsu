# `.hyper` / `.race` of an unbounded sequence stay lazy

`(1..*).map(*+0).hyper.map(*+1).head(3)` threw `Cannot .hyper a lazy list`, because `hyper`
and `race` were in the lazy step's reify table and mutsu's `HyperSeq` holds a settled element
list. Rakudo wraps the source iterator and batches it on demand.

A lazy `LazyList` now carries a `hyper_kind` (`Plain`, `Hyper`, `Race`). `Seq`/`List`
`hyper` and `race` are lazy *pulls*: for a sequence that does not end they return the same
lazy list tagged as `HyperSeq`/`RaceSeq`, so its stages stream like any lazy `map`/`grep` and
`.WHAT` reports the right class. A sequence that ends is still reified into the settled
`HyperSeq`. Nothing is batched across threads, as before. The `HyperSeq`/`RaceSeq` rows
decline a `LazyList` receiver so the lazy step answers.

Part of #11276; closes #13918. Pinned in `t/collections/lazy-seq/lazy-hyper-seq.t`.
