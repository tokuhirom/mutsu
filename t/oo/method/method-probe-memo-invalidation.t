use Test;

# The per-`(class, method)` MRO probes on the method-call path (does a user
# method or an accessor win, which levels declare the method, is it native)
# are memoized (#9172). A declaration made after a call has already filled
# the memo must still be seen by the next call.
#
# All expectations below were measured against rakudo v2026.07.

plan 12;

class A { method m { 'A' } }
class B is A { }
class C is B { }

my $c = C.new;
is $c.m, 'A', 'inherited method resolves';
my $name = 'm';
is $c."$name"(), 'A', 'and resolves through a dynamic call';

B.^add_method('m', method { 'B' });
B.^compose;
is $c.m, 'B', 'a method added to an ancestor after a call wins';
is $c."$name"(), 'B', 'and wins through a dynamic call too';

C.^add_method('m', method { 'C' });
C.^compose;
is $c.m, 'C', 'a method added through the MOP after a call wins';
is $c."$name"(), 'C', 'and wins through a dynamic call too';

# An accessor probed before the class grows a same-named method.
class P { has $.v = 'attr' }
class Q is P { }
my $q = Q.new;
is $q.v, 'attr', 'the inherited accessor answers';
Q.^add_method('v', method { 'method' });
Q.^compose;
is $q.v, 'method', 'a method added after an accessor call shadows it';
is Q.new.v, 'method', 'for a fresh instance too';

# A method that did not exist when first probed.
class R { }
my $r = R.new;
nok $r.can('late'), 'no method yet';
R.^add_method('late', method { 'late' });
R.^compose;
is $r.late, 'late', 'a method added after a failed probe is found';
my $late = 'late';
is $r."$late"(), 'late', 'and through a dynamic call';
