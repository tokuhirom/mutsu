use Test;

# The scalar declared in a `for` body must not be visible to an `EVAL` after
# the loop (#13531): the body's routine hoist used to note the body's `my`
# names in the ENCLOSING scope's EVAL-visible declarations.
plan 4;

for <x y> { my $k = 1; }
throws-like { EVAL('$k') }, X::Undeclared, 'for body scalar is undeclared after the loop';

for <x y> { my ($a1, $b1) = 1, 2; }
throws-like { EVAL('$b1') }, X::Undeclared, 'for body list declaration is undeclared after the loop';

for <x y> { my $kept = 1; }
my $kept = 5;
is EVAL('$kept'), 5, 'a same-named outer scalar declared later stays visible';

sub f { for <x y> { my $q = 1; }; EVAL('$q') }
throws-like { f() }, X::Undeclared, 'same inside a routine';
