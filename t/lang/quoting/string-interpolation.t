use Test;
plan 7;

my $name = "world";
is "hello $name", "hello world", "simple interpolation";
is "value: $name!", "value: world!", "interpolation with trailing chars";
my $x = 42;
is "num $x end", "num 42 end", "numeric var interpolation";
is 'no $name interp', 'no $name interp', "single quotes no interpolation";

# Meta-method interpolation (.^name, .?method, etc.)
my $str = "hello";
is "$str.^name()", "Str", ".^name() meta-method in string interpolation";
my $h = {a => 1};
is "$h.^name()", "Hash", ".^name() on hash in string interpolation";
is "$str.uc()", "HELLO", "regular method call still works";
