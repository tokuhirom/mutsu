# An empty `Map`'s `.raku` is `Map.new`, and its `.gist` keeps `Map.new(())`

`Map.new.raku` printed `Map.new(())` where Rakudo prints the bare `Map.new`:
Rakudo drops the argument list of `.raku` when a Map has no pairs, and keeps it
in `.gist` (so `say Map.new` is `Map.new(())`). Anything that printed a match's
`.hash` with no named captures showed the difference
([#12040](https://github.com/tokuhirom/mutsu/issues/12040)).

The `.raku` text of an immutable Map was assembled at three separate sites
(the value renderer, the constrained-hash bypass and the VM's `gist|raku|perl`
override), each with its own `format!("Map.new(({}))")`. They now share
`raku_map_new` (`src/value/raku_repr.rs`), which renders the bare `Map.new` for
an empty Map, and the VM override's `, ` join (which Rakudo's `.raku` does not
use) is gone with the copy it belonged to.

Checking `.gist` against `raku` turned up the mirror-image bug in that same
override: it answered `Map.new` for an empty Map's explicit `.gist` method call
while `say $map` (the other renderer) already printed `Map.new(())`. Both
spellings of `.gist` now agree with Rakudo.

Pinned by `t/collections/hash/hash-empty-map-raku.t`, whose expectations were
taken from `raku` (the reported repro, a scalar-held and a `Map`-typed
container, nested in a `List`, the `EVAL` round trip, non-empty Maps, and the
neighbouring empty `Hash`/`Set`/`Bag`/`Mix`, which already matched).
