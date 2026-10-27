use lib 't/lib';
use Test;

# A Junction argument to an imported multi (a captured candidate family bound
# by the `use`) is autothreaded when no candidate takes it whole, as it is for
# a multi declared in the file. (Found with Red's `compare(any($a, $b), $c)`,
# a `where` clause of `Optimizer::AND`: "Cannot resolve caller
# compare(Junction:D, ...)".)

use JunctionImportedMulti;

plan 11;

my $x = Leaf.new(:n(1));
my $y = Leaf.new(:n(2));

is same($x, $y), False, 'proto + multi: plain call';
is-deeply same(any($x, $y), $y), any(False, True), 'proto + multi: Junction first argument';
is-deeply same($x, any($x, $y)), any(True, False), 'proto + multi: Junction second argument';
ok so(same(any($x, $y), $y)), 'so of the threaded result';

is-deeply both(any($x, $y), $y), any(3, 4), 'multi without a proto';
is both(all($x, $y), $y).gist, 'all(3, 4)', 'an all Junction';
is both($x, $y), 3, 'multi: plain call';

is takes-any(any(1, 2)), 'mu', 'a candidate that takes Mu receives the Junction whole';
is-deeply takes-any(any(1, 2)).WHAT, Str, 'the answer is a plain Str';

my @r = (same(any($x, $y), $x), same($x, $x));
is @r[1], True, 'later calls are unaffected';
is-deeply same(any($y, $x), $y), any(True, False), 'eigenstate order follows the Junction';

done-testing;
