use v6;
use Test;

# The shapes the compiled regex engine used to decline to the tree walk
# (ADR-0135 Slice E): a name on a separated quantifier's own token, a
# `** { … }` count with a separator or over a nullable body, and a
# backreference inside a `&` conjunction or a `~` goal. Every expected value is
# rakudo 2026.09's.

plan 30;

# A name on the separated token itself is applied per atom.
{
    my $m = "a,b,c" ~~ /<alpha>+ % ","/;
    is $m<alpha>».Str.join("|"), 'a|b|c', '<alpha>+ % "," names each atom';
    is $m<alpha>.^name, 'Array', '... as a list';
    is ("a,b" ~~ /<x=alpha>+ % ","/)<x>».Str.join("|"), 'a|b', 'an angle alias';
    is ("a" ~~ /<alpha>+ % ","/)<alpha>.elems, 1, 'one atom is still a list of one';
    my $t = "a,b,c," ~~ /<alpha>+ %% ","/;
    is ~$t, 'a,b,c,', '%% takes the trailing separator';
    is $t<alpha>.elems, 3, '... and names the three atoms';
    is ("" ~~ /<digit>* % ","/)<digit>.raku, '[]', 'zero atoms: an empty list';
    is ("abc" ~~ / <alpha>+ % <alpha> /)<alpha>».Str.join("|"), 'a|b|c',
        'the separator files under the same name, in match order';
    my $d = "a1b2c" ~~ / <alpha>+ % <digit> /;
    is $d<alpha>».Str.join("|") ~ ' ' ~ $d<digit>».Str.join("|"), 'a|b|c 1|2',
        'atom and separator names side by side';
    my $p = "a1b2" ~~ / <alpha>+ %% (\d) /;
    is $p<alpha>».Str.join("|") ~ ' ' ~ $p[0]».Str.join("|"), 'a|b 1|2',
        'a positional separator capture next to the named atoms';
}

# `** { … } % sep`.
{
    is ~("a,a,a,a" ~~ / a ** {2} % "," /), 'a,a', 'a fixed count';
    is ~("a,a,a,a" ~~ / a ** {2..3} % "," /), 'a,a,a', 'a range takes its maximum';
    is ("a,a,a,a" ~~ / a ** {0} % "," /).raku,
        'Match.new(:orig("a,a,a,a"), :from(0), :pos(0))', 'zero: an empty match';
    nok "b" ~~ / a ** {1} % "," /, 'below the minimum fails';
    is ~("a,a,a,a" ~~ / a ** {^3} % "," /), 'a,a', 'an exclusive range';
    is ~("a,a,a,b" ~~ / a ** {1..*} % "," ',b' /), 'a,a,a,b', 'an open range gives back for what follows';
    is ~("a,a,a," ~~ / a ** {2} %% "," /), 'a,a,', '%% with a count';
    my $n = 3;
    is ~("x1x2x3x4" ~~ / [x \d] ** {$n} % "" /), 'x1x2x3', 'the count reads a lexical';
    grammar G { token TOP { <h> ** { 3 } % ':' }; token h { \d+ } }
    is G.parse("1:22:333")<h>».Str.join("|"), '1|22|333', 'in a token';
    grammar G2 { token TOP { <h> ** { 2 } % ':' }; token h { \d+ } }
    nok G2.parse("1:22:333"), 'a token commits to its count';
    is ~("a,a,a" ~~ / :r a ** {1..2} % "," /), 'a,a', 'ratcheted';
}

# A nullable body under `** { … }`.
{
    grammar N { token TOP { <h> ** {4} }; token h { \d? } }
    is ~N.parse("12"), '12', 'a count over a callee that can match empty';
}

# A backreference in a conjunction branch sees the enclosing captures.
{
    ok 'aa' ~~ / $<x>=(\w) [ $<x> && \w ] /, 'in the first branch';
    nok 'ab' ~~ / $<x>=(\w) [ $<x> && \w ] /, '... and fails when it differs';
    ok 'aa' ~~ / $<x>=(\w) [ \w && $<x> ] /, 'in a later branch';
    nok 'ab' ~~ / $<x>=(\w) [ \w && $<x> ] /, '... and fails when it differs';
    ok 'abab' ~~ / (\w)(\w) [ $0 $1 && .. ] /, 'positional backreferences';
}

# Both sides of a `~` goal see the enclosing captures.
{
    ok 'a(a)' ~~ / $<x>=(\w) '(' ~ ')' $<x> /, 'a backreference inside the goal';
    nok 'a(b)' ~~ / $<x>=(\w) '(' ~ ')' $<x> /, '... and fails when it differs';
    my $seen;
    'a(b)' ~~ / (\w) '(' ~ ')' [ \w { $seen = ~$0 } ] /;
    is $seen, 'a', 'code inside the goal sees the enclosing $0';
}
