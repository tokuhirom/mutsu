use Test;

# An element assignment yields the element's container, so `++` / `--` on it
# write that element (#18884). The subscript is evaluated once.
plan 9;

my @a;
(@a[0] = 1)++;
is-deeply @a, [2], '(@a[0] = 1)++ increments the element';

my @b;
my $i = 0;
(@b[$i++] = 5)++;
is "{@b} $i", '6 1', 'a subscript with a side effect runs once';

my %h;
(%h<k> = 1)++;
is-deeply %h, {k => 2}, '(%h<k> = 1)++ increments the value';

my %g;
my $k = 'x';
is (%g{$k} = 3)++, 3, 'the postfix form returns the assigned value';
is-deeply %g, {x => 4}, '...and leaves the incremented one';

my @c = 1, 2;
is ++(@c[1] = 10), 11, 'the prefix form returns the incremented value';

my @d;
(@d[0] = 3)--;
is-deeply @d, [2], 'a decrement';

my %seen;
for <a b> -> $key { (%seen{$key} = 0)++ }
is %seen.sort.join(','), "a\t1,b\t1", 'inside a loop, a fresh subscript each time';

my $r = [1, 2];
($r[0] = 7)++;
is-deeply $r, [8, 2], 'through an itemized array';
