use Test;

# A plain `$var` atom compares the variable's string in place and files the
# match-time name `$name` on the stack; values and names of every shape must
# still match as they did (#16614).
plan 9;

my $needle = "code=";
ok "info code=12" ~~ / $needle \d+ /, 'a Str variable before a quantified class';
my $dot = "a.b";
ok "xa.by" ~~ / $dot /, 'regex metacharacters in the value are literal';
nok "xaxby" ~~ / $dot /, 'and do not act as a pattern';
my $n = 42;
ok "id 42!" ~~ / $n /, 'an Int variable';
my $upper = "CODE=";
ok "info code=1" ~~ / :i $upper \d /, 'a Str variable under :i';
my $uni = "caf\x[e9]";
ok "un caf\x[e9] noir" ~~ / $uni /, 'a non-ASCII value';
my $long-name-that-is-long-enough-to-be-seventy-characters-or-so-abcdefghijk = "z";
ok "xyz" ~~ / $long-name-that-is-long-enough-to-be-seventy-characters-or-so-abcdefghijk /, 'a long variable name';

my $re = rx/ \d+ /;
ok "a12" ~~ / a $re /, 'a Regex value is still a pattern';
my $c = 0;
for 1..3 { my $v = $_; $c++ if "k$_" ~~ / k $v / }
is $c, 3, 'the variable is read afresh on every match';
