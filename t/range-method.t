use Test;
plan 3;

my @a = 1, 2, 3;
is @a.Range, 0..^3, '.Range on array returns index range';

is "abcd".Range, 0..^4, '.Range on string returns char index range';

is (2..5).Range, 2..5, '.Range on Range returns itself';
