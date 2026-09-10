use Test;
plan 5;

my @a = 1, 2, 3;
my $v = @a.pop;
is $v, 3, "pop returns last";
is @a.elems, 2, "pop reduces size";

@a.unshift(0);
is @a[0], 0, "unshift adds to front";

my $first = @a.shift;
is $first, 0, "shift returns first";
is @a.elems, 2, "shift reduces size";
