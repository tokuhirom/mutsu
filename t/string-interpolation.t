use Test;
plan 4;

my $name = "world";
is "hello $name", "hello world", "simple interpolation";
is "value: $name!", "value: world!", "interpolation with trailing chars";
my $x = 42;
is "num $x end", "num 42 end", "numeric var interpolation";
is 'no $name interp', 'no $name interp', "single quotes no interpolation";
