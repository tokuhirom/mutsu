# `cas` no longer writes a lane retired mid-`cas`

`cas` on a scalar that takes the legacy name-keyed atomic lane (for example a
`my Node $head` still holding its type object) resolved the lane's value slot
once, at entry. If another thread retired that lane while the `cas` block was
running — and because the lane is keyed by bare name process-wide, an
unrelated `my $x = 1; $x = 2` in a worker is enough — the swap was written
into the retired slot and lost: the variable kept its old value.

`cas` now checks, under the same write lock as its compare, that the root
store still maps the variable to the slot it resolved. A retired lane fails
that attempt and the next attempt re-resolves the variable's current lane,
the way Raku's `cas` re-reads its container on every retry. This resolves
ADR-0062's "Not addressed" item 2 (#9921) and is pinned by
`t/concurrency/thread-lock/atomic-lane-retired-mid-cas.t`.
