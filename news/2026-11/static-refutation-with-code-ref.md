# A routine taken as &f is still refuted at CHECK time

The CHECK-time refutation pass (#18874) used to leave alone any routine the
unit also wrote as `&f`. rakudo refutes `sub f(Int $x) {}; my $r = &f;
f("a")` all the same, even after `&f.wrap(...)`, and now mutsu does too. Only
a `&f` variable or parameter, which shadows the routine where it is in scope,
still keeps the pass away from calls to that name.
