# `hidden-from-backtrace` applies to subs and implicit proto frames

Subroutine frames now honor `is hidden-from-backtrace`, including multi candidates.
An onlystar `proto` no longer adds a duplicate visible frame when it dispatches to
a candidate. This matches Rakudo's backtrace output for both proto subs and proto
methods (#13524).
