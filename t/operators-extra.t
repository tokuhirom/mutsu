use Test;
plan 14;

# ? prefix (boolean coercion)
is ?1, True, '?1 is True';
is ?0, False, '?0 is False';
is ?"hello", True, '?"hello" is True';
is ?"", False, '?"" is False';

# ^ upto operator
is-deeply (^5).list, [0, 1, 2, 3, 4], '^5 produces 0..^5';

# <=> numeric comparison
is (1 <=> 2), -1, '1 <=> 2 is -1';
is (2 <=> 2), 0, '2 <=> 2 is 0';
is (3 <=> 2), 1, '3 <=> 2 is 1';

# cmp (polymorphic)
is (1 cmp 2), -1, '1 cmp 2 is -1';
is ("a" cmp "b"), -1, '"a" cmp "b" is -1';

# eqv
ok (1 eqv 1), '1 eqv 1';
nok (1 eqv 2), 'not 1 eqv 2';

# leg
is ("abc" leg "abd"), -1, '"abc" leg "abd"';
is ("abc" leg "abc"), 0, '"abc" leg "abc"';
