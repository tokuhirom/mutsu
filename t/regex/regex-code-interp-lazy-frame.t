use v6;
use Test;

# The pattern a `$( … )` / `@( … )` interpolation yields is matched lazily, as
# rakudo matches an interpolated regex: code inside it runs only on the paths
# the match takes, and none of its captures are kept. Every expected value is
# rakudo 2026.09's.

plan 14;

my $n = 0;
my $r = rx/ a+ { $n++ } /;

ok "aaab" ~~ / $($r) b /, '$( Regex ) matches';
is $n, 1, '... running its code once, at the end the match takes';

$n = 0;
ok "aaaa" ~~ / $($r) a /, 'backtracking into the interpolated regex';
is $n, 2, '... runs its code once more';

$n = 0;
ok "aaab" ~~ / @( $r, 'x' ) b /, '@( … ) with a Regex element';
is $n, 1, '... runs that element\'s code once';

is ("ab12" ~~ / $( rx{ (\w) (\w) } ) (\d+) /).list».Str.join("|"), '12',
    'the interpolated regex\'s positional captures are not kept';
nok ("xay" ~~ / x $( rx{ $<k>=(\w) } ) y /)<k>:exists,
    '... nor its named ones';
is ("ab" ~~ / @( rx{ (\w) } ) b /).list.elems, 0, '... nor a list element\'s';

is ~("abc" ~~ / :r $( rx{ \w+ } ) /), 'abc', 'ratcheted';
nok "abc" ~~ / :r $( rx{ \w+ } ) c /, '... commits to the first end';
ok "abc" ~~ / $( rx{ \w+ } ) c /, 'without ratchet it gives back';

is ~("foo bar" ~~ / $('foo') ' ' $( "b" ~ "ar" ) /), 'foo bar', 'string results are literals';
my @w = <cat ca>;
is ~("cat" ~~ / @( @w ) t? /), 'cat', 'a list of strings';
