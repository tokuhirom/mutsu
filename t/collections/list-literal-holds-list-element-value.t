use Test;

plan 5;

# A List literal holding an element of an immutable List holds the value
# itself (a List element has no container), so it flattens into a slurpy.
sub rs(*@i) { @i.elems }
sub f(@x) { (@x[0], 1).head }
is rs(f((<b c>, 1))), 2, 'element of a List param flattens';
sub g(@x) { given (@x[0], @x[1]) { rs($_.head) } }
is g((<b c>, <A C>)), 2, 'through given on a list literal';

# An Array element is still held by its container.
my @a = 1, 2;
my (\p, \q) := (@a[0], @a[1]);
p = 9;
is-deeply @a, [9, 2], 'an Array element is still aliased';
my @c = 1, 2;
my $m = (@c[0], 5);
@c[0] = 7;
is-deeply $m, (7, 5), 'a later write to the Array element is visible';
my @b;
my $l = (@b[5],);
is @b.elems, 0, 'and a missing Array element is not vivified';
