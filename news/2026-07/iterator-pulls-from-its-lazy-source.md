# An `Iterator` pulls from its lazy source instead of a frozen prefix

`gather { take 1; take 2 }.iterator.pull-one` answered the string
`IterationEnd` instead of `1`. The sentinel was leaking out as an ordinary
value, so a consumer that checks for it saw an empty sequence and one that does
not saw the literal string — `DBIish`'s `t/05-mock.rakutest` test 12 compared it
against a row and reported `expected: 'a b 1', got: 'IterationEnd'`.

`.iterator` built its `Iterator` instance by snapshotting
`value_to_list(target)` into an `items` array plus a zero index, and
`runtime/iterator_protocol.rs` stepped over that array. For an already
materialised source that is fine — `(1,2,3).Seq`, an `Array`, even
`(1..3).map(*+1)` were all correct. For a lazy one the snapshot is only whatever
prefix has been produced so far, and for a `gather` that has never been forced
that is nothing at all, so the very first step was already past the end. Forcing
the source at construction time would have fixed the gather and hung on
`gather { loop { take 1 } }`.

The instance now keeps the lazy source alongside the prefix, and each protocol
call tops the prefix up from it before stepping — bounded by what that
particular call needs. `pull-one` asks for one more element than the cursor,
`push-exactly(@a, n)` for `n`, `skip-at-least(n)` for `n`; only `push-all` and
`sink-all`, which consume the source by definition, force it to exhaustion.
`push-until-lazy` deliberately asks for nothing, since stopping at a lazy
boundary is its entire purpose. An infinite `gather` therefore still yields
element after element without materialising, and the mutating dispatch path
keeps the grown prefix on the instance so repeated pulls do not re-force from
the start.

Pinned by `t/iterator-pull-from-lazy-source.t`, which passes unchanged under
rakudo. `DBIish`'s `t/05-mock.rakutest` reaches raku parity at 16/16 with this
and the bare-adverb parse fix that landed alongside it, taking the database
battery to 7 of 9 files at parity.
