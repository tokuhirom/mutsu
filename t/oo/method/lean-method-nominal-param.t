# A method whose positional parameter is typed with a user class runs in the
# lean frame (#19293): an instance of exactly that class passes there, and
# everything else -- a subclass instance, the type object, a junction, a
# wrong type, a role or subset type -- must still get the general binder's
# answer. Each call is made many times so the method is hot (planned).
use Test;

plan 11;

class P {
    has $.x;
    method diff(P $o) { $!x - $o.x }
}
class Q is P { }
class Other { has $.x = 99 }

my $p = P.new(x => 10);
my $s = 0;
$s += $p.diff(P.new(x => $_)) for ^200;
is $s, 2000 - 19900, 'exact-class arguments';

$s = 0;
$s += $p.diff(Q.new(x => $_)) for ^200;
is $s, 2000 - 19900, 'a subclass instance passes';

throws-like { $p.diff(Other.new) for ^5 }, X::TypeCheck::Binding::Parameter,
    'a wrong-class instance fails the binding';
throws-like { $p.diff(42) }, X::TypeCheck::Binding::Parameter, 'a non-instance fails';
my $err = (try { $p.diff(Other.new) }) // $!;
like $err.message, /'Type check failed in binding'.*'$o'/, 'the error names the parameter';

# The type object passes a non-definite nominal type; its accessor then fails.
throws-like { $p.diff(P) }, Exception, 'the type object binds, as in the general binder';

my $j = $p.diff(P.new(x => 1) | P.new(x => 2));
is $j.^name, 'Junction', 'a junction argument autothreads';
ok ?($j == 8) && ?($j == 9) && !?($j == 7), 'with the right values';

role R { has $.x = 5 }
class WithR does R { }
class UsesR { method m(R $r) { $r.x * 2 } }
my $u = UsesR.new;
my $t = 0;
$t += $u.m(WithR.new) for ^200;
is $t, 2000, 'a role-typed parameter still binds a composing instance';

subset Small of P where *.x < 5;
class UsesSubset { method m(Small $s) { $s.x } }
my $us = UsesSubset.new;
my $v = 0;
$v += $us.m(P.new(x => 1)) for ^200;
is $v, 200, 'a subset-typed parameter';
throws-like { $us.m(P.new(x => 50)) }, X::TypeCheck::Binding::Parameter,
    'the subset constraint is still checked';
