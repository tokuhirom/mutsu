use Test;
use lib $*PROGRAM.parent(2).add('lib-term-vs-routine');

# From the CSS::Properties distribution (CSS::Units): `role CSS::Units[\dimension,
# \units] { method dimension{dimension} }` next to an exported `sub dimension`.
# A bare name is the sigilless term when one is in scope; only `name(...)` is the
# routine.
plan 6;

use Imp;
use Foo::RU :ALL;
sub g(\dimension) { dimension }
is g(5), 5, 'a sub parameter at statement start';
sub h(\dimension) { dimension ~ '' }
is h(5), '5', 'a sub parameter before an infix';
my &f = -> \dimension { dimension ~ '' };
is f(5), '5', 'a pointy parameter';
is dimension(1), 'routine', 'name(...) still calls the routine';

my $v = 3px;
is $v.units, 'px', 'role parameter units';
is $v.dimension.^name, 'Lengths', 'role parameter bound to an enum type beats the exported routine';
