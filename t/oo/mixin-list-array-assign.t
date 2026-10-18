use Test;

# A role mixed into a list is still that list: assigned to an `@` variable it
# flattens into the array's elements, while a scalar holding it stays one item
# (every answer checked against Rakudo).

plan 7;

role R { }

my @c = (1, 2, 3) but R;
is @c.elems, 3, 'a mixed-in List flattens on @-assignment';
is @c.gist, '[1 2 3]', 'its elements are the plain list elements';

my @d;
@d = (1, 2, 3) but R;
is @d.elems, 3, 'also on a plain (non-declaring) assignment';

my @f = [1, 2] but R;
is @f.elems, 2, 'a mixed-in Array flattens';

my @g = $[1, 2] but R;
is @g.elems, 2, 'the mix-in is a new object, not the itemized container';

my $l = (1, 2, 3) but R;
my @e = $l;
is @e.elems, 1, 'a scalar holding a mixed-in list stays one item';

my @b = (3, 1, 2).sort.list but R;
is @b.gist, '[1 2 3]', 'gist of an array assigned a mixed-in list';
