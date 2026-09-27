# An Array's `.keys`/`.values`/`.kv`/`.pairs`/`.antipairs`/`.batch` and `.combinations`/`.permutations` are lazy

These methods used to build their whole result at the call. A consumer that stopped early
still paid for all of it. `@a.pairs.head(3)` promoted every slot of a 200 000-element
array to a container, and `(^10).permutations.head` built 3.6 million arrays.

Now each returns a lazy `Seq` over a native iterator, `ListGen` in `src/value/list_gen.rs`.
This follows Rakudo, where every one of these methods is a `Seq.new` over an iterator. The
Array views read the array through a live index cursor, so `my $v = @a.values; @a.push(4)`
shows the pushed element. `.keys` is the exception: it fixes its count at the call, the way
Rakudo's count-only iterator over `@a.elems` does. On a mutable Array, `.values`/`.pairs`/`.kv`
still hand out rw element containers, but they now promote one slot per element pulled.
`.combinations` and `.permutations` step a lexicographic index vector over a snapshot of the
invocant, in Rakudo's order.

The iterator plugs into the deferred-source machinery that `Str.comb`/`.lines`/`.words`
already used. That source is now `SeqSource::Pure(PureCursor)`, which holds either a `Str`
cursor or a list iterator. The Seq is cut on its first read, so every existing consumer
still sees an ordinary Seq, and a consuming `.head(n)`/`.first` or a subscript pulls only
the prefix it needs.

Measured on a debug build, 100 rounds of `@a.keys.head(3); @a.pairs.head(3); @a.kv.head(3)`
over 200 000 elements went from 101.7 s to 0.066 s, and doubling the array no longer
changes the time. `(^9).permutations.head` plus `(^300).combinations(2).head(2)` went from
1.36 s to 0.002 s. This is part of #9158. The eager `.map`/`.grep`, `for`, `.rotor`, `.tree`
and `.flat` paths are still open there.
