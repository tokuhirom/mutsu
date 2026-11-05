use Test;

# FAILGOAL receives the goal the way rakudo's RakuAST deparse spells it: a
# quoted literal is double-quoted (`']'` is `"]"`), a word-only one is bare
# (`'ab'` is `ab`), `\` and `"` are escaped; the source's trailing whitespace is
# kept.

plan 10;

my $n = 0;
sub goal-of(Str $lit, Str $input) {
    my $name = 'G' ~ ++$n;
    my $code = 'grammar ' ~ $name ~ ' { token TOP { "x" ~ ' ~ $lit
             ~ ' \w+ }; method FAILGOAL($g) { die "<$g>" } }; try '
             ~ $name ~ '.parse("' ~ $input ~ '"); $!.message';
    EVAL $code
}

is goal-of(q['('], 'xbad'), '<"(" >', 'a single-quoted bracket';
is goal-of('"]"', 'xbad'), '<"]" >', 'a double-quoted bracket';
is goal-of(q['ab'], 'xbad'), '<ab >', 'a word-only literal is bare';
is goal-of(q['1'], 'xbad'), '<1 >', 'a digit is bare';
is goal-of(q['a_b'], 'xbad'), '<a_b >', 'underscore is a word character';
is goal-of(q['a b'], 'xbad'), '<"a b" >', 'a literal with a space is quoted';
is goal-of(q['a-b'], 'xbad'), '<"a-b" >', 'a literal with a hyphen is quoted';
is goal-of(q['+'], 'xbad'), '<"+" >', 'a symbol is quoted';

grammar Tight { token TOP { '(' ~ ')'\w+ }; method FAILGOAL($g) { die "<$g>" } }
try Tight.parse('(bad');
is $!.message, '<")">', 'no trailing whitespace in the source, none in the goal';

grammar Named { token TOP { '(' ~ <close> \w+ }; token close { ')' }; method FAILGOAL($g) { die "<$g>" } }
try Named.parse('(bad');
is $!.message, '<<close> >', 'a named goal is kept as written';
