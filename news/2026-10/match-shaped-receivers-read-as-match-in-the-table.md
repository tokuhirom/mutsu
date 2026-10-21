# A grammar cursor reads as a `Match` in the method table

A grammar cursor (and any instance marked as a match) has no dispatch shape of
its own, but its structure is a `Match`'s. The table's dispatch now resolves
such a receiver to `Match`'s rows, so the 0-argument cascade no longer carries
the lazy-Match and `Match` receiver blocks (`match_owner_row` is gone). Second
half of #14328 (the exception half landed in #14334).
