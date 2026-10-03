use Test;

plan 6;

my $x;
is (try { $x = 9 }), 9, 'try block with a trailing assignment yields the value';
is $x, 9, 'the assignment happened';
my $r = try { $x = 10 };
is $r, 10, 'the value can be assigned onward';
is (try { 9 }), 9, 'plain tail expression still works';
is (do { $x = 11 }), 11, 'do block with a trailing assignment';
my $y;
is (try { $y = 1; $y = 2 }), 2, 'last of several assignments';
