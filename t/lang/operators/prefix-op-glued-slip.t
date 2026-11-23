use Test;

# A symbolic prefix glued to a prefix `|` slip: `?|@a` is `?(|@a)`. In term
# position the infix `?|` / `+|` / `~|` cannot start, so these were parse
# errors ("Confused") in mutsu; rakudo reads them as prefix + slip.

plan 9;

my @z = 0, 0;
is ?|(0, 1), True, '?|(...)';
is ?|@z, True, '?|@a: a non-empty slip boolifies to True';
is +|(1, 2), 2, '+|(...) numifies the slip (its element count)';
is ~|(1, 2), '1 2', '~|(...) stringifies the slip';
is -|(1, 2), -2, '-|(...) negates the slip';
is !|(0, 1), False, '!|(...) already worked';

is (1 ?| 0), True, 'the infix ?| is unchanged';
is (3 +| 4), 7, 'the infix +| is unchanged';
is ("a" ~| "b"), 'c', 'the infix ~| is unchanged';
