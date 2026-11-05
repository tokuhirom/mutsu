use Test;

# ADR-12917 direction 2: an interpolated `$var` is read when the cursor reaches
# it, so a code block earlier in the pattern can change what it matches.
plan 9;

my $x = "a";
is ~("xb" ~~ / x { $x = "b" } $x /), 'xb', 'a block assigns the variable read after it';

my $y = "a";
is ~("xa" ~~ / x { 1 } $y /), 'xa', 'an untouched variable still matches its value';

my $r = /\d+/;
is ~("ab12" ~~ / ab { 1 } $r /), 'ab12', 'a Regex value after a block is still entered as a pattern';

my $z = "a";
nok ("xa" ~~ / x { $z = "b" } $z /).so, 'the new value is the one matched';

my $n = 1;
is ~("x2" ~~ / x { $n = 2 } $n /), 'x2', 'a numeric value is read after the block too';

my $q = "a";
is ~("xb" ~~ / x '{' ** 0 { $q = "b" } $q /), 'xb', 'a quoted brace is not a block';

my $early = "a";
is ~("ab" ~~ / $early { 1 } b /), 'ab', 'before the block the value is what it was';

# A loop re-enters an atom written before the block, after the block ran.
my $l = "a";
is ~("aab" ~~ / [ $l { $l = "b" } ]+ /), 'a', 'a loop re-reads a scalar written before its block';

my $m = "a"; my $k = 0;
is ~("abbb" ~~ / [ $m { $m = "b" if $k++ == 0 } ]+ /), 'abbb', 'the loop sees the updated value on later passes';
