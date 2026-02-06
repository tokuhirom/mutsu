use Test;
plan 8;

# x - string replication
is "ab" x 3, "ababab", 'string x 3';
is "ha" x 1, "ha", 'string x 1';
is "ha" x 0, "", 'string x 0';

# x with number stringification
is 42 x 2, "4242", 'int x 2 stringifies';

# xx - list replication
my @a = 1 xx 3;
is @a.join(","), "1,1,1", 'xx replicates into list';

my @b = "x" xx 4;
is @b.join(","), "x,x,x,x", 'xx with string';

my @c = 0 xx 0;
is @c.join(","), "", 'xx with 0 gives empty list';

# x in expressions
is "a" x 2 ~ "b" x 2, "aabb", 'x and ~ combined';
