use Test;

plan 10;

# `[&TERM]` as an infix takes any `&`-term, not only a plain `&name`, and
# calls it with the two operands. The meta prefixes apply too. (#9327)

my &f = &infix:<+>;
is (1 [&f] 2), 3, 'a plain &name still works';

my $f = &infix:<+>;
is (1 [&($f)] 2), 3, '&(...) contextualizer';
is (1 [&infix:<->] 2), -1, 'a qualified &infix:<...> name';
is (5 [&( -> $a, $b { $a * $b })] 3), 15, 'a literal block inside &(...)';

is (1 R[&infix:<->] 2), 1, 'R reverses the operands';
is (1 R[&($f)] 2), 3, 'R with &(...)';
is-deeply ((1, 2) X[&($f)] (3, 4)), (4, 5, 5, 6), 'X crosses with the callable';
is-deeply ((1, 2) Z[&($f)] (3, 4)), (4, 6), 'Z zips with the callable';

is (1 [&($f)] 2 [&($f)] 3), 6, 'the operator chains';

# The Terminal::UI shape.
my $count = 5;
my $current = 4;
my $op = &infix:<+>;
$current = ($current [&($op)] 1) % $count;
is $current, 0, '($current [&($op)] 1) % $count';
