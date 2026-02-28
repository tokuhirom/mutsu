use Test;
plan 16;

# ? prefix (boolean coercion)
is ?1, True, '?1 is True';
is ?0, False, '?0 is False';
is ?"hello", True, '?"hello" is True';
is ?"", False, '?"" is False';

# ^ upto operator
is-deeply (^5).list, [0, 1, 2, 3, 4], '^5 produces 0..^5';
my @vals = <a b c>;
is-deeply (^@vals.elems).list, [0, 1, 2], '^@array.elems parses and evaluates as 0..^N';
my $sum = 0;
for ^@vals.elems -> $i { $sum = $sum + $i; }
is $sum, 3, '^@array.elems works as for-loop iterable';

# <=> numeric comparison
is (1 <=> 2), Order::Less, '1 <=> 2 is Less';
is (2 <=> 2), Order::Same, '2 <=> 2 is Same';
is (3 <=> 2), Order::More, '3 <=> 2 is More';

# cmp (polymorphic)
is (1 cmp 2), Order::Less, '1 cmp 2 is Less';
is ("a" cmp "b"), Order::Less, '"a" cmp "b" is Less';

# eqv
ok (1 eqv 1), '1 eqv 1';
nok (1 eqv 2), 'not 1 eqv 2';

# leg
is ("abc" leg "abd"), Order::Less, '"abc" leg "abd"';
is ("abc" leg "abc"), Order::Same, '"abc" leg "abc"';
