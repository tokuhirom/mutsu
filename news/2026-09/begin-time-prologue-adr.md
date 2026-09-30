# ADR-0134: a real BEGIN time for mutsu

mutsu parses a whole compilation unit, compiles it, then runs it, so it never
had a BEGIN time. Three approximations covered the common cases: a
sub-interpreter hoist for simple top-level BEGINs, a phaser reorder pass, and a
per-site memo for value-form BEGINs. Measured against rakudo, eleven programs
still diverged. For example, `my $c = True; BEGIN say $c.raku` prints `Any` on
rakudo and `Bool::True` on mutsu. A BEGIN inside an uncalled sub never ran, and
one inside a `for` loop ran on every iteration. `use Foo:if($c)` read `$c` at
run time (#9919).

[ADR-0134](../../docs/adr/0134-begin-time-prologue.md) (Accepted) fixes the
contract. Every BEGIN-time effect runs once, in source order, before the
unit's CHECK, INIT and mainline. The effects are `BEGIN`, `constant`, `use`
with its `:if`, and `will begin`. Each effect sees lexicals in their static
state, and whatever it stores becomes their starting value. The mechanism is
a compiled prologue in the unit's own frame, with static cells for inner-scope
lexicals. It lands in three slices, and it retires the hoist, the BEGIN half
of the reorder pass and the run-time `:if` guard.
