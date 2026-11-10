use Test;
plan 8;

my @r = %(id => 1), %(id => 2), %(id => 3);
my @x = @r[0], @r[1];
my @y = @r[1], @r[2];
is (@x ∪ @y).elems, 3, '∪ keeps each Hash of an Array as one element';
is (@x ∩ @y).elems, 1, '∩';
is (@x ∖ @y).elems, 1, '∖';
is (@x ⊖ @y).elems, 2, '⊖';
is (@x ∪ @x).elems, 2, '∪ of an Array with itself';
is @x.Set.elems, 2, '.Set agrees';
my %h = a => 1, b => 2;
my %g = c => 3;
is (%h, %g).Set.elems, 3, 'a bare List still flattens its Hashes';
is-deeply ((%h, %g) (|) (%g,)).elems, 3, '(|) of lists';
