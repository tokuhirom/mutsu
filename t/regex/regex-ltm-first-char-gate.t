# An LTM alternation whose branches each begin with a character test is
# ranked by the character in front of it when at most one branch can begin
# with it, without measuring the branches (#12917). These pin that the match
# (which branch wins, what it captures, which code runs) stays rakudo's, in the
# cases the shortcut takes and in the ones it must leave to the measurement.
# Values are rakudo's.
use Test;

plan 13;

# Disjoint first characters: the shortcut decides.
is ~('xb12' ~~ / [ a \w+ | b \d+ | c ] /), 'b12', 'the only branch that can start wins';
is ~('xc9' ~~ / [ a \w+ | b \d+ | c ] /), 'c', 'a short branch wins when it is the only one';
nok 'xyz' ~~ / ^ [ a | b | c ] /, 'no branch can start';

# Several branches can start: the longest still wins.
is ~('abcd' ~~ / [ a | abc | ab ] /), 'abc', 'longest of the branches that can start';
is ~('acz' ~~ / [ ab | ac ] /), 'ac', 'two branches start alike, one goes on';

# The admitting branch fails after its first character.
nok 'xz' ~~ / ^ [ xy | z ] $ /, 'the only starter fails later';

# Code in the branches runs only for the branch that is tried and goes on.
{
    my @seen;
    'b' ~~ / [ a { @seen.push('a') } | b { @seen.push('b') } ] /;
    is @seen.join(','), 'b', 'only the matching branch runs its block';
}

# Branches that start with rule calls.
grammar G {
    token TOP { [ <num> | <word> | <quoted> ] }
    token num { \d+ }
    token word { <[a..z]>+ }
    token quoted { "'" <-[']>* "'" }
}
is G.parse("'hi there'")<quoted>.Str, "'hi there'", 'a quoted branch through a call';
is G.parse('42')<num>.Str, '42', 'a number branch through a call';
is G.parse('abc')<word>.Str, 'abc', 'a word branch through a call';

# A sequential alternation inside a branch keeps its own order.
is ~('ab' ~~ / [ a [ b || bc ] | x ] /), 'ab', 'a || inside the winning branch';

# Branches over the whole subject, repeatedly (the YAML key shape).
grammar K {
    token TOP { <key>+ % ' ' }
    token key { <plain> | <single> | <double> }
    token plain { <[a..z]> <[a..z0..9]>* }
    token single { "'" <-[']>* "'" }
    token double { '"' <-["]>* '"' }
}
my $m = K.parse(q{key1 'two' "three" four});
is $m<key>.elems, 4, 'every key parses';
is $m<key>[2]<double>.Str, '"three"', 'and the double-quoted one is the double branch';
