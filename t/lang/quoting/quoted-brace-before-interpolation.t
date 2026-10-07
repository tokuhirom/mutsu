use Test;

plan 5;

# Reduced from App::Stouch's template substitution: a single-quoted `'{{'` in a
# regex was taken for the start of a code block, so the `$k` after it was never
# interpolated and the match failed.
my $k = 'T';
is ('a {{T}} b' ~~ / '{{' $k '}}' /).Str, '{{T}}', 'double brace, scalar, double brace';
is ('{T' ~~ / '{' $k /).Str, '{T', 'single brace then scalar';

my $s = 'a {{T}} b';
$s ~~ s:g/ '{{' $k '}}' /X/;
is $s, 'a X b', 's:g with quoted braces around an interpolated scalar';

my %param = Target_1 => '1', Target_2 => 'Two';
my $str = 'a {{Target_1}} b {{Target_2}} c';
for %param.kv -> $name, $v {
    $str ~~ s:g/ '{{' $name '}}' /$v/;
}
is $str, 'a 1 b Two c', 'template substitution loop';

# An apostrophe-free char class and a real code block are left alone.
ok 'a{b' ~~ / a '{' b /, 'quoted brace without interpolation';
