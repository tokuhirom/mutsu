# A sub that reads a unit-level lexical takes the light call paths

A named sub declared at a program's or module's top level that reads or writes
a lexical of that scope (`my $cache; sub get($k) { $cache{$k} }`, ADR-0024)
used to be excluded from every light call path: the fast, positional-light,
light, multi-light, plan-light and proto-winner-light paths. Each call went
through the general named-call binder. The exclusion dated from when those
paths pushed no routine frame for the captured-variable resolver to key on;
ADR-0037 Slice 1 had removed that reason, so the exclusion only cost speed.

It is gone. On a profiling build, a top-level sub reading one unit-level `my`
fell from 28.1k to 10.0k instructions per call; the same sub without the free
variable is 7.9k. This is the first slice of ADR-18244 stage B (O1, static
lexical addressing), carried by #12529.

Removing it exposed two steps only the general path performed, and both now
happen on the light paths too:

- `sub r($a is rw) { $outer := $a }` bound `$outer` to a copy of the argument.
  A light call aliases an `is rw` argument by putting the caller's container
  cell in the parameter's slot, and the bind looked the source up by name in
  env, where no cell is mirrored. The bind now joins the slot's cell.
- A routine with no locals that declares a `my package` leaked the package
  name to its caller on a warm (fast-path) call: the return merge counted only
  the package's mangled storage name as the routine's own, not the bare name it
  binds.
