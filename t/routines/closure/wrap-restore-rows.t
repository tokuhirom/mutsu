use Test;

# The methods of the runtime's result objects are rows (ADR-11276 §9.160):
# `Routine::WrapHandle.restore`, `HyperConfiguration`'s `batch` and `degree`,
# and `WalkList`'s.

plan 8;

sub f { 'plain' }
my $h = &f.wrap(-> { 'wrapped' });
is f(), 'wrapped', 'the wrapper runs';
ok $h.restore, 'restore answers True the first time';
is f(), 'plain', 'and unwraps';
nok $h.restore, 'a second restore answers False';
ok $h.can('restore'), 'a WrapHandle can restore';

my $c = (1..10).hyper(:batch(3), :degree(2)).configuration;
is $c.batch, 3, 'HyperConfiguration.batch';
is $c.degree, 2, 'HyperConfiguration.degree';

class A { method m { 'a' } }
class B is A { method m { 'b' } }
is B.new.WALK('m').reverse.invoke.join(','), 'a,b', 'WalkList.reverse / .invoke';
