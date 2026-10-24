use Test;

# `Any.AT-POS(0)` is the invocant, so a role mixed into a scalar survives
# `[0]` (CSS::Properties: `$css.background-position[0][0].type`).
plan 6;

role R { method type { 'r' } }
my $s = 'top' but R;
is $s[0].type, 'r', 'a Str with a role keeps it under [0]';
is $s[0][0].type, 'r', 'and under [0][0]';
my $n = 5 but R;
is $n[0].type, 'r', 'an Int with a role keeps it under [0]';
is $s[0].^name, 'Str+{R}', 'the mixin type name';
ok $s[1] ~~ Failure, '[1] is out of range';
my @a = $s, $s;
is @a[0][0].type, 'r', 'inside an array';
