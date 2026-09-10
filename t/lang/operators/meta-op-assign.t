use Test;
plan 4;

my @a = 1, 2;
my @b = 10, 20;
@a [X+]= @b;
is @a.join(" "), "11 21 12 22", 'X+ op= assignment';

my @c = 1, 2, 3;
my @d = 10, 20, 30;
@c [Z+]= @d;
is @c.join(" "), "11 22 33", 'Z+ op= assignment';

my $x = 5;
$x [R-]= 3;
is $x, -2, 'R- op= assignment';

my @m1 = 5, 1, 8;
my @m2 = 3, 7, 2;
@m1 [Zmin]= @m2;
is @m1.join(" "), "3 1 2", 'Zmin op= assignment';
