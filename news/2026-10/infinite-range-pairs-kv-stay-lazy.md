# `.pairs`/`.kv`/`.antipairs` over an infinite range stay lazy

The lazy index-pipe stage for `.pairs`/`.antipairs`/`.kv` only recognised a
`LazyList` invocant, so an infinite integer range (`(1..*).pairs`, `^Inf`)
fell through to the eager path: it reified a 100,000-element prefix
(~190 MiB, 0.65 s) to answer `.pairs[^3]`, and `.is-lazy` said `False` where
Rakudo says `True`.

The three call sites (`CallMethod`, `CallMethodMut` and the runtime dispatcher)
now share one helper, `LazyList::index_pipe_method`, which also turns an
infinite integer range into its arithmetic sequence and pipes over that.
`(1..*).pairs[^3]` now takes ~10 ms and reports `.is-lazy` as `True`.
