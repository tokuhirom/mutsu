# `Supply.delayed` over a finite supply no longer blocks `.tap`

`Supply.from-list(1, 2, 3).delayed(0.1).tap(...)` slept before replaying each
value, so `.tap` returned after n × delay with every value already delivered.
Rakudo cues each value on the scheduler, so `.tap` returns at once (#18696).

A positive `delayed` over any source is now the per-tap delay stage that
#18608 introduced for live sources. Each tap gets its own delay worker, which
listens on a fresh supplier, and the source is replayed into that supplier only
once the stage's own tap is in place, so a finite source's values and its
`done` are never lost. `done` still follows the last value.
