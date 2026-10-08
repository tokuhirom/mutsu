# Promise.in passes its delay to a custom scheduler unchanged

`Promise.in($seconds, :scheduler($s))` used to coerce `$seconds` to a `Num` before calling
`$s.cue(:in(...))`. A virtual-clock scheduler that computes `Instant + $in` therefore picked up
f64 noise (`Duration.new(9.999999903)` instead of `10`). The delay is now handed over as given, as
Rakudo does. Found by the `Test::Time` ecosystem distribution, whose only test file now passes
under mutsu (6/6, was 4/6).
