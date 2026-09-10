use Test;

plan 1;

my @x = ^10;
my @y = 2..3;

is @y ?? (@x[@y] :v) !! @x, "2 3",
    'parenthesized ternary branch accepts :v adverb on subscript';
