# A refuted call is reported after the unit's CHECK phasers

rakudo's optimizer, which rejects a call that can never bind ("Calling f(Str)
will never work ..."), runs after the unit's CHECK phasers:
`CHECK { say "check" }; sub f(Int $x) {}; f("a")` prints `check` and then the
compile error, and a CHECK phaser that exits ends the run before the error is
reported. mutsu's CHECK-time refutation pass (#18874) reported the error
before any CHECK phaser ran, which broke the `static` distribution: its own
CHECK-time verdict (`[static: mismatch]`) never printed.

A unit with a CHECK phase (CHECK phasers, or BEGIN-time effects that may
queue check code) now holds the refutation until that phase ends, after the
phasers, the queued check code and the `$*CU.add-sorry` diagnostics.
