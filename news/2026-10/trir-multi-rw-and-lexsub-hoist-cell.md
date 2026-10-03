# `is rw` multi candidates from typed routine frames; nested subs keep one cell

Three bugs found while running BSON::Simple's test suite:

- A routine whose frame runs on the typed tier (TRIR) could not reach a
  `multi` candidate with an `is rw` parameter. In
  `sub t($x) { my $r = 0; k($x, $r) }`, the variable was handed over by value,
  so `multi k($a, $p is rw)` was ruled out ("Cannot resolve caller"). The
  generic call site now passes a container at every position where *any*
  candidate declares `is rw`.
- A declaration used as an argument (`f($b, my $pos = 0)`) now binds an
  `is rw` parameter from a TRIR frame too, the way `f($pos)` does. This is the
  shape of BSON::Simple's `bson-decode($bson, my $pos = 0)`.
- A routine-nested `my sub` may be called from a closure that was created
  before the sub's in-sequence registration. That closure captured a cell the
  variable's later declaration replaced, so the sub could read the caller's
  same-named variable instead of its own. The hoisted registration now seeds
  the cell the declaration adopts (the #9911 mechanism).

The remaining BSON::Simple failure, a stale free-variable read when the
recursive sub also passes the variable to `=>`, is #11433.
