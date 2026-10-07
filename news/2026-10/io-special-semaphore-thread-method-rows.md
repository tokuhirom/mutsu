# `IO::Special`, `Semaphore` and `Thread` methods are method-table rows

The first owners the oracle snapshot of Rakudo's method tables lacked (ADR-11276 slice 3E, part 4) now have
recognition rows, snapshot lines and 29 registered rows: `IO::Special`'s stream queries, `Semaphore`'s
`acquire`/`try_acquire`/`release` and `Thread`'s `id`/`name`/`Str`/`gist`/`finish`... Their hand-written
dispatch `match`es and `.^can` name lists are gone, so `.^methods` and `.^can` read the same table that
dispatches. Two answers moved to Rakudo's: `IO::Special.gist` is the `raku` form, and `Thread.gist` is
`Immortal Thread #id (name)`.
