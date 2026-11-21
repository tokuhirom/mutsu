use Test;

# #18529: `.comb` with a regex runs the regex's code blocks wherever they
# sit -- inside an alternation or a group too, not only at the top level.

plan 10;

my $n = 0;
is-deeply "aa".comb(/[ a { $n++ } | b ]/).List, <a a>, 'alternation: the matches';
is $n, 2, 'alternation: the code block ran once per match';

$n = 0;
my $t = token { [ a { $n++ } | b ] };
is-deeply "aa".comb($t).List, <a a>, 'anonymous token: the matches';
is $n, 2, 'anonymous token: the code block ran once per match';

$n = 0;
is-deeply "aab".comb(/ (a { $n++ })+ /).List, ('aa',), 'quantified capture group';
is $n, 2, 'the code block in the group ran for each repetition';

$n = 0;
is-deeply "ab".comb(/ a <?{ $n++; True }> /).List, ('a',), 'code assertion';
is $n, 1, 'the assertion ran once';

my @taken = gather "abc".comb(/ [ <[ab]> { take ~$/ } | c ] /).eager;
is-deeply @taken.List, <a b>, 'take inside an alternation';

$n = 0;
"aa".comb(/ a { $n++ } /);
is $n, 2, 'a top-level code block still runs';
