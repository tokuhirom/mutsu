# `RakuAST::CompUnit.add-check-code`

`$*CU.add-check-code({ ... })`, the undocumented rakudo method `static::CLI`
calls from its `sub EXPORT` (maintainer-approved in #15513), queues a closure on
the unit being compiled. The unit runs the queue at its CHECK phase: after its
CHECK phasers, last queued first, before INIT, with no arguments. Inside a
module's `sub EXPORT`, `$*CU` is now the *importing* unit rather than the
module's own. `RakuAST::CompUnit:D` (a smiley on a core RakuAST type) is a
valid term. `use static::CLI` now runs its CHECK-time analysis and stops at
the next undocumented method it needs, `.fileline` (filed separately).
