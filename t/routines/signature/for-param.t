use Test;
plan 7;

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

my $arrayitem = [1, 2, 3];
my $single_count = 0;
for ($arrayitem) { $single_count++; }
is $single_count, 1, "for (\$scalar_array) iterates once";

$single_count = 0;
for $arrayitem { $single_count++; }
is $single_count, 1, "for \$scalar_array iterates once";

my $a = 42;
for ($a) -> $v is rw { $v++; }
is $a, 43, "for (\$scalar) -> \$v is rw writes back";

$a = 42;
for $a -> $v is rw { $v++; }
is $a, 43, "for \$scalar -> \$v is rw writes back";
