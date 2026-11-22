use Test;

# The type object of a class that inherits from `Mu` only is no `Any`: an
# `Any` parameter, an untyped parameter, an `Any` variable and an `Any` multi
# candidate reject it, as `~~ Any` already did (#18804).

plan 15;

class M is Mu {}
class C {}
my $t = M;

sub h(Any $x) { 'ok' }
sub f($x)     { 'ok' }
sub m(Mu $x)  { 'ok' }

nok $t ~~ Any, 'M ~~ Any is False';
ok  $t ~~ Mu,  'M ~~ Mu is True';
ok  $t ~~ M,   'M ~~ M is True';

throws-like { h($t) }, X::TypeCheck::Binding, 'an Any parameter rejects M';
throws-like { f($t) }, X::TypeCheck::Binding, 'an untyped parameter rejects M';
throws-like { f($t) }, X::TypeCheck::Binding, 'an untyped parameter rejects M on a repeat call';
is m($t), 'ok', 'a Mu parameter takes M';
is f(C),  'ok', 'an untyped parameter still takes an ordinary class type object';

my Any $a;
throws-like { $a = $t }, X::TypeCheck::Assignment, 'an Any variable rejects M';

multi mm(Any $x) { 'any' }
multi mm(Mu $x)  { 'mu' }
is mm($t), 'mu',  'M dispatches to the Mu candidate';
is mm(C),  'any', 'an ordinary class dispatches to the Any candidate';

class N is M {}
nok N ~~ Any, 'a subclass of a Mu-only class is no Any either';
my $n = N;
throws-like { f($n) }, X::TypeCheck::Binding, 'an untyped parameter rejects the subclass';

# A built-in class's type object is still an Any.
ok X::TypeCheck::Binding ~~ Any, 'a core exception type object ~~ Any';
is f(X::AdHoc), 'ok', 'an untyped parameter takes a core exception type object';
