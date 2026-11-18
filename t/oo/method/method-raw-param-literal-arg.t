use Test;

# A method's `$x is raw` parameter called with no variable argument at all
# (`$o.m(1)`, `$o.m(1 + 2)`) binds on the fast method binder: the call site
# records no argument-source table then, and the binder still has to bind the
# value read-only and alias any container or Proxy it is handed.  The same
# method called with a variable, an element or a Proxy keeps aliasing it.

plan 16;

class A {
    method m(Mu $x is raw) { $x }
    method set(Mu $x is raw) { $x = 42; $x }
    method var(Mu $x is raw) { $x.VAR.^name }
    method typed(Int $x is raw) { $x }
}
my $a = A.new;

is $a.m(1), 1, 'a literal binds by value';
is $a.var(1), 'Int', 'a literal has no Scalar behind it';
is $a.m(1 + 2), 3, 'an expression binds by value';
is $a.var(1 + 2), 'Int', 'an expression has no Scalar behind it';
ok $a.m(Mu) =:= Mu, 'a Mu type object passes a Mu constraint';
dies-ok { $a.set(1) }, 'assigning to a literal-bound raw parameter dies';
is-deeply $a.m((1, 2)), (1, 2), 'a List value binds as itself';
is $a.var([1, 2]), 'Array', 'an Array value binds as itself';
is-deeply (^3).map({ $a.m($_) }).List, (0, 1, 2), 'the topic binds per call';
dies-ok { $a.typed('x') }, 'a typed raw parameter still rejects a literal of the wrong type';

my $v = 5;
$a.set($v);
is $v, 42, 'a variable argument is aliased';
is $a.var($v), 'Scalar', 'a variable argument keeps its Scalar';

my @arr = 1, 2;
$a.set(@arr[0]);
is-deeply @arr, [42, 2], 'an element argument is aliased';

my %h = a => 1;
$a.set(%h<a>);
is-deeply %h, %(a => 42), 'a hash element argument is aliased';

my $stored;
my $p := Proxy.new(FETCH => { 7 }, STORE => -> $, $n { $stored = $n });
$a.set($p);
is $stored, 42, 'a Proxy argument stores through STORE';

my $total = 0;
$total += $a.m($_) for ^1000;
is $total, 499500, 'a hot literal-argument call keeps binding correctly';
