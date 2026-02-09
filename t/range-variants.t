use Test;
plan 8;

# ^.. (exclusive start)
my @a = (1 ^.. 5).list;
is @a.elems, 4, '^.. excludes start';
is @a[0], 2, '^.. starts from start+1';
is @a[3], 5, '^.. includes end';

# ^..^ (exclusive both)
my @b = (1 ^..^ 5).list;
is @b.elems, 3, '^..^ excludes both';
is @b[0], 2, '^..^ starts from start+1';
is @b[2], 4, '^..^ excludes end';

# iteration with for
my $sum = 0;
for 0 ^.. 4 -> $i { $sum = $sum + $i; }
is $sum, 10, '^.. works in for loop (1+2+3+4=10)';

my $sum2 = 0;
for 0 ^..^ 4 -> $i { $sum2 = $sum2 + $i; }
is $sum2, 6, '^..^ works in for loop (1+2+3=6)';
