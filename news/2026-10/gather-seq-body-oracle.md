# Rakudo's plain-gather behaviour is pinned before the SeqBody adapter

ADR-11678 makes a plain `gather` a `Seq` whose body pulls from the existing
gather coroutine (`SeqSource::Gather`). Step 0 of its plan lands first, on its
own: `t/collections/lazy-seq/gather-seq-body-oracle.t` checks every row of the
ADR's divergence table against Rakudo v2026.09 — itemization (`$((2,).Seq)`),
identity (`$g === $g`), consumption (`X::Seq::Consumed` after `.head`, `.List`
or `.iterator`) and resuming after a prefix — as `todo`, next to the behaviours
that already match and must stay so: an infinite gather in a list, Pair or Hash
is never run at construction, `for` and `.iterator` interleave with the body,
`.cache` and `@$g` re-read, and `take` of a Slip or a nested gather.

It also records the GC baseline the adapter must keep (399 of 400 unforced
self-referencing gathers reclaimed, as in Rakudo), and found an unrelated bug:
a `return` in a gather body forced inside its own routine dies instead of
returning (#13003).
