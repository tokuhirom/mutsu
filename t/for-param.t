use Test;
plan 3;

my $sum = 0;
for 1..3 -> $x { $sum = $sum + $x; }
is $sum, 6, "for range -> param";

my @a = 10, 20, 30;
my $total = 0;
for @a -> $item { $total = $total + $item; }
is $total, 60, "for array -> param";

my $count = 0;
for 1..5 { $count++; }
is $count, 5, "for without param uses $_";
