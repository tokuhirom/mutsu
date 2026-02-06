use Test;
plan 2;

my $x = 0;
repeat { $x++; } while $x < 3;
is $x, 3, "repeat while";

my $y = 5;
repeat { $y++; } until $y > 7;
is $y, 8, "repeat until";
