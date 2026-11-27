# Rakuish t/82-errors-recall test 52 is accepted until the backtrace shape matches rakudo

Rakuish 0.141.1 added `t/82-errors-recall.t`. After #19242 gave EVAL its own backtrace frame,
87 of its 88 assertions match rakudo.

The remaining one, test 52, counts the lines Rakuish's `errors full` replays from
`$ex.backtrace.list`. That count depends on rakudo's internal backtrace shape:

- `throw` and `die` frames in `SETTING::` files;
- a frame for a `do { }` block;
- module frame files spelled `"/path/Mod.rakumod (Mod)"`.

Matching it needs a frame push on a hot path and a decision on how `$?FILE` is spelled, so it is
tracked as #19244 for after the 2026-11-28 release. Until then the file is an accepted divergence
(maintainer decision 2026-11-27).
