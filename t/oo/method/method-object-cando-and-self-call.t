use Test;

# Distribution: JSON::RPC (server picks `$app.^find_method($name)`, filters
# with `.cando`, then calls the candidate with the application as invocant).

plan 14;

class A {
    has Int $.count is rw;
    multi method foo(Int $x) { 1 }
    multi method foo(Str $x) { 2 }
    method bar($a) { 3 }
    method notify($n) { $.count = $n }
    method via-self($n) { self.count = $n }
    method can($fish) { 'meow' }
}

my $a = A.new;

# Method.cando filters candidates by a capture whose first element is the invocant
my $foo = A.^find_method('foo');
is $foo.cando(\(A, 1)).elems, 1, 'multi: Int candidate';
is $foo.cando(\(A, 'x')).elems, 1, 'multi: Str candidate';
is $foo.cando(\(A, 1.5)).elems, 0, 'multi: no candidate for Rat';
my $bar = A.^find_method('bar');
is $bar.cando(\(A, 1)).elems, 1, 'single method accepts matching arity';
is $bar.cando(\(A)).elems, 0, 'single method rejects missing argument';

# a Method object called with an explicit invocant binds self
my $n = A.^find_method('notify');
$n($a, 7);
is $a.count, 7, '$.attr assignment through a called Method object';
A.^find_method('via-self')($a, 8);
is $a.count, 8, 'self.attr assignment through a called Method object';
my @c = $n.cando(\($a, 9));
@c.shift()($a, 9);
is $a.count, 9, 'a cando candidate is callable with its invocant';

# built-in metamodel names resolve to Routine objects; a user method wins
my class B { }
ok B.^find_method('can') ~~ Routine, 'built-in name finds a Routine';
is B.^find_method('can').WHAT.gist, '(Method)', 'it is a Method';
ok A.^find_method('can') ~~ Routine, 'user-defined can is a Routine';
is A.^find_method('can')($a, 'tuna'), 'meow', 'user-defined can is the one found';
sub takes-routine(Routine $r) { 'ok' }
is takes-routine(B.^find_method('isa')), 'ok', 'Routine-typed parameter accepts it';
is A.^find_method('nope').defined, False, 'missing method is undefined';
