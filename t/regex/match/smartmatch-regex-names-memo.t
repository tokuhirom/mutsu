use Test;

# The variables a `~~` publishes for its regex are looked up once per pattern
# text; a repeated match must still see each iteration's value.

plan 7;

my $needle = 'a';
my @hits;
for <a b c> -> $n {
    $needle = $n;
    @hits.push(so 'xbx' ~~ / $needle /);
}
is @hits.join(','), 'False,True,False', 'a loop sees the current value of the variable';

sub probe($v) { 'k7-7' ~~ / $v \d / ?? ~$/ !! 'no' }
is probe('k'), 'k7', 'same pattern text, first call';
is probe('x'), 'no', 'same pattern text, other value';
is probe('-'), '-7', 'same pattern text, third value';

my $re = / $needle /;
$needle = 'q';
ok 'aq' ~~ $re, 'a regex value names the variable';
$needle = 'z';
nok 'aq' ~~ $re, 'and reads it again at the next match';

my $x = 5;
ok 'a5' ~~ / <?{ $x == 5 }> a /, 'a code block reads a local';

# vim: ft=raku
