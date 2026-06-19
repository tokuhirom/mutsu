use Test;

# A Junction passed as a named argument that is collected by a slurpy hash
# (`*%h`, `+%h`, the implicit `%_`) must NOT auto-thread the call: the slurpy
# captures it as a raw value. Only a Junction bound to an ordinary (Any-typed)
# parameter auto-threads. Regression: mutsu used to auto-thread named junction
# args with no explicit matching named param, even when a `*%h` would catch them
# — so `check-parts($p, $d, :basename(｢\｣|｢/｣), ...)` ran 2×N times.

plan 4;

sub slurpy-hash($x, *%h) { %h }
is-deeply slurpy-hash('a', :k(1|2))<k>, (1|2),
    'junction into *%h is captured raw (no autothread)';

my @calls;
sub count-slurpy($x, *%h) { @calls.push: %h<k>; 'r' }
count-slurpy('a', :k(1|2));
is @calls.elems, 1, '*%h call runs once, not once-per-eigenstate';

## A junction bound to an ordinary Any parameter still auto-threads.
my @pos;
sub pos-any($x, $y) { @pos.push: $y; 'r' }
pos-any('a', 1|2);
is @pos.elems, 2, 'positional junction into $y still auto-threads';

# A junction named arg matching an explicit Any-typed named param auto-threads.
my @named;
sub named-any($x, :$y) { @named.push: $y; 'r' }
named-any('a', :y(1|2));
is @named.elems, 2, 'explicit Any named param still auto-threads';
