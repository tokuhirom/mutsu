use Test;

# An Int interpolated into a Str is written by the shared decimal routine
# (`str_prim::IntDecimal`). It must render exactly what `.Str` renders.

plan 5;

my @n = 0, 7, -7, 10, 99, 100, -100, 12345, 1000000,
    9223372036854775807, -9223372036854775807 - 1;
is @n.map({ "<$_>" }).join, @n.map({ '<' ~ .Str ~ '>' }).join, 'matches .Str';
my $i = -9223372036854775807 - 1;
is "$i", '-9223372036854775808', 'i64 minimum';
is "x{10 ** 3}y", 'x1000y', 'block interpolation';
my $sum = '';
for 95..105 { $sum ~= "$_," }
is $sum, '95,96,97,98,99,100,101,102,103,104,105,', 'two- and three-digit boundary';
is "{-1}{-10}", '-1-10', 'negative pieces';
