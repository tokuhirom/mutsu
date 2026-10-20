use Test;

# From the PDF distribution (ASCII85 decode): `(@a[$i] *= 85) += $v`.
plan 5;

my uint32 @b;
@b[0] = 2;
(@b[0] *= 85) += 7;
is @b[0], 177, 'native array element';

my @a = 2;
(@a[0] *= 85) += 7;
is-deeply @a, [177], 'array element';

my %h = a => 2;
(%h<a> *= 4) += 1;
is-deeply %h, {a => 9}, 'hash element';

my $i = 0;
my @c = 1, 1;
(@c[$i++] += 1) *= 5;
is-deeply @c, [10, 1], 'subscript evaluated once';
is $i, 1, 'side effect ran once';
