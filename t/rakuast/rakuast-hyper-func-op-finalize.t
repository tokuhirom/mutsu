use v6;
use Test;

plan 4;

# `@a >>[&f]<< @b`, a hyper operator over a function, is carried through
# finalization as the parser built it (ADR-15760 slice 2).

sub plus($x, $y) { $x + $y }
my @a = 1, 2, 3;
my @b = 10, 20, 30;

is (@a >>[&plus]<< @b).raku, '[11, 22, 33]', 'both sides hyper';
is (@a >>[&plus]>> 5).raku, '[6, 7, 8]', 'a scalar on the right';
is (5 <<[&plus]<< @a).raku, '[6, 7, 8]', 'a scalar on the left';
is (@a >>[&infix:<*>]<< @b).raku, '[10, 40, 90]', 'a core infix function';
