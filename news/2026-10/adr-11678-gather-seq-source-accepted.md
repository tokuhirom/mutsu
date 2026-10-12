# ADR-11678 accepted: a plain gather becomes a deferred Seq source

ADR-11678 is revised and accepted as the design for the remaining gather divergences
(#11678). The re-measurement shows rendering was fixed by #12366; what still differs from
Rakudo is scalar itemization, identity, consumption and resuming after a prefix. The decision
is a `SeqSource::Gather` adapter that holds the existing `Gc<LazyList>` as one value edge, so
the coroutine, its GC tracing and resume stay in `LazyList` while identity, consumption, cache
and `$` itemization come from `SeqBody`. The first slice covers plain gathers only, with
streaming for `for`/`.iterator`/prefix reads and a pure-reader audit as requirements. ADR-0034's
exclusion of `LazyList`-backed gathers is amended accordingly.
