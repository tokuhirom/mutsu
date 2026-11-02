use Test;

# #15940: `min` / `max` of two objects order them as `cmp` does, by their
# `Str` (rakudo), not by the structural order of the instances.

plan 8;

class Box { has $.v; method Str { ~$!v } }
my $x = Box.new(:v(2));
my $y = Box.new(:v(1));

is ($x max $y).v, 2, 'max of two objects with Str';
is ($x min $y).v, 1, 'min of two objects with Str';
is ($y max $x).v, 2, 'max, operands swapped';
is ($y min $x).v, 1, 'min, operands swapped';
is ($x cmp $y), More, 'cmp agrees';

is (3 max 5), 5, 'Int max unchanged';
is ("a" min "b"), "a", 'Str min unchanged';
is ((1, 2) max (1, 3)).raku, '(1, 3)', 'List max unchanged';
