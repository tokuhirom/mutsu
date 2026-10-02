use v6;
use Test;

# A `where` clause on a parameter nested in a sub-signature is checked by the
# binder like a top-level one (#10989).

plan 6;

my $c = 0;
sub f(*@ ($x where { $c++; True })) { }
f(1);
is $c, 1, 'slurpy sub-signature where predicate runs once';

sub g(*@ ($x where { False })) { "g called" }
throws-like { g(1) }, X::TypeCheck::Binding::Parameter,
    'slurpy sub-signature where rejects';

sub h(@ ($x where { False })) { "h called" }
throws-like { h([1]) }, X::TypeCheck::Binding::Parameter,
    'positional sub-signature where rejects';

sub i(@ ($x where * > 0, $y)) { "i $x $y" }
is i([1, 2]), 'i 1 2', 'a passing where binds';
throws-like { i([0, 2]) }, X::TypeCheck::Binding::Parameter,
    'a failing WhateverCode where rejects';

sub j(@ ($x where { $x == 1 })) { $x }
is j([1]), 1, 'the where block sees the nested parameter';
