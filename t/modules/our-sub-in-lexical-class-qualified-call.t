use Test;

# An `our sub` of a lexical class (`my class X { our sub f }`) is callable as
# `X::f(...)`, like one of a package class (#16653).

plan 7;

my class X { our sub f { 7 }; our sub g($a) { $a * 2 } };
class Y { our sub h { 8 } }

is X::f(), 7, 'qualified call, no arguments';
is X::g(4), 8, 'qualified call with an argument';
is X::f, 7, 'bare qualified name';
is &X::f(), 7, 'code-variable call';
is X::<&f>(), 7, 'stash lookup call';
is X.WHO<&g>(5), 10, 'WHO lookup call';
is Y::h(), 8, 'a package class is unchanged';

done-testing;
