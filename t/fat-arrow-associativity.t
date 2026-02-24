use Test;

plan 6;

my $right := 1 => 2 => 3 => 4;
is($right.raku, '1 => 2 => 3 => 4', 'fat arrow chain is right-associative');
isa-ok($right.value, Pair, 'right-associative chain nests Pair on value side');

my $left := ((1 => 2) => 3) => 4;
is($left.raku, '((1 => 2) => 3) => 4', 'left-nested pair key keeps parentheses');

is($right, 1 => 2 => 3 => 4, 'fat arrow chain works in call arguments');
is($left.key.key, 1 => 2, 'left-nested key remains a nested pair');
is($left.value, 4, 'left-nested value is preserved');
