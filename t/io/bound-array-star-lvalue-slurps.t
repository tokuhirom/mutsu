use Test;

# The first array of a list assignment takes every remaining value, whether
# or not it was bound with `:=` (ADR-0097 retired the bound-length record that
# truncated a bound array to the element count it was bound with).

plan 5;

my @b := [0, 0];
(*, @b) = 1, 2, 3, 4;
is-deeply @b, [2, 3, 4], 'a bound array slurps the rest of a star lvalue';

my @c = 0, 0;
(*, @c) = 1, 2, 3, 4;
is-deeply @c, [2, 3, 4], 'so does an assigned one';

my @d := [0, 0];
@d.push(9);
(*, @d) = 1, 2, 3, 4, 5;
is-deeply @d, [2, 3, 4, 5], 'a grown bound array too';

my @e := [7, 8];
is +@e, 2, 'a bound array declaration still binds';
is (+my @f := 1, 2, 3), 3, 'a bound array declaration in expression position';
