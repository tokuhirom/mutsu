use Test;

# An untyped parameter's implicit `Any` check of a type object is remembered
# per type while the type registry is unchanged (#17376). Repeat calls, a
# type declared after the first call, and two lexical classes that share a
# name must each still get their own answer.

plan 9;

sub f($x) { $x.^name }
sub g(\x) { x.^name }

is f(Any), 'Any', 'Any type object binds an untyped parameter';
is f(Int), 'Int', 'core type object binds';
is g(Str), 'Str', 'sigilless parameter binds a type object';

class C {}
is (f(C) for ^3).join(','), 'C,C,C', 'repeat calls with a class type object';

my $mu = Mu;
nok (try f($mu)).defined, 'Mu type object is still rejected after the memo filled';
nok (try f($mu)).defined, 'Mu type object is rejected on a repeat call';

class D {}
is f(D), 'D', 'a class declared after the first calls binds';

{
    my class L {}
    is f(L), 'L', 'a lexical class binds';
}
{
    my class L {}
    is (f(L) for ^2).join(','), 'L,L', 'a second lexical class of the same name binds';
}
