use Test;

plan 8;

# Storing an Int-valued enum (an Order from `cmp`, a Bool) into a native
# `int` variable stores its integer (#17337).

my int $c;
$c = 2 cmp 1;
is $c, 1, 'Order::More stores as 1';
is $c.^name, 'Int', 'the stored value is an Int';
$c = 1 cmp 2;
is $c, -1, 'Order::Less stores as -1';
is ($c = 3 cmp 3), 0, 'the assignment expression answers the stored integer';
$c = True;
is $c, 1, 'True stores as 1';
$c = False;
is $c, 0, 'False stores as 0';

my int $n = 0;
my &by = &[cmp];
for ^4 -> $i { $n = $n + ($c = by($i, 2)) }
is $n, -1, 'a loop of stored comparisons sums their integers';

my $plain;
$plain = 2 cmp 1;
is $plain.^name, 'Order', 'an untyped variable keeps the enum';
