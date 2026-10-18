use Test;

# A routine a routine's body declares (`sub h`, `my sub h`) is what a bare
# `h()` there calls, even when an outer scope binds a `my &h` (#13855).
# Checked against rakudo.
plan 5;

my &h = sub { 'outer' };
sub f { sub h { 'inner' }; h() }
sub g { my sub h { 'my-inner' }; h() }
sub k { h() }
sub m($x) { sub h($y) { "inner-$y" }; h($x) }

is f(), 'inner', 'a nested sub shadows the outer my &h';
is g(), 'my-inner', 'so does a nested my sub';
is k(), 'outer', 'a routine that declares none calls the outer binding';
is m(3), 'inner-3', 'with arguments';
is h(), 'outer', 'the outer binding is unchanged';
