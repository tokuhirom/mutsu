use v6;
use Test;

# The interpreter's own `__mutsu_*` bookkeeping keys are never user package
# variables (#15688): writing a sigilless binding must not try to resolve the
# key's head as a package alias, and the binding semantics stay unchanged.

plan 6;

my \x = 5;
is x, 5, 'a sigilless binding reads back';
throws-like { x = 6 }, X::Assignment::RO, 'a sigilless binding stays read-only';

sub f(\v) { v }
is f(7), 7, 'a sigilless parameter binds';

class C {
    method m(\a, \b) { a + b }
}
is C.new.m(1, 2), 3, 'a sigilless method parameter binds';

my \y = 1;
my $sum = 0;
$sum += y for ^10;
is $sum, 10, 'a sigilless binding is stable across a loop';

package Alias { our $v = 4 }
constant E = Alias;
is E::<$v>, 4, 'a constant package alias still resolves a qualified variable';
