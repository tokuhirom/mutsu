use Test;

# The no-capture regex matcher (`regex_match_end_from_in_pkg`) drives every
# consumer that only needs match *extents*: `.comb(/rx/)`, `.split(/rx/)`,
# `.subst(..., :g)`, and the lookahead/lookbehind assertions the capturing
# matcher delegates to. It used to ignore `RegexToken::frugal` outright, so a
# non-greedy quantifier matched greedily there while the same pattern under
# `~~` (capturing matcher) was frugal. `"a \"b\" c \"d\"".comb(/ \" .*? \" /)`
# returned ONE match spanning both quoted runs instead of two.

plan 16;

my $s = 'a "b" c "d"';

is-deeply $s.comb(/ \" .*? \" /).List, ('"b"', '"d"'),
    '.comb with a frugal .*? yields one match per quoted run';
is-deeply $s.comb(/\".*?\"/).List, ('"b"', '"d"'),
    '.comb with a frugal .*? (no whitespace in the pattern)';
is ($s ~~ / \" .*? \" /).Str, '"b"',
    '~~ agrees with .comb on the same frugal pattern';
is-deeply $s.match(/\".*?\"/, :g).map(*.Str).List, ('"b"', '"d"'),
    '.match(:g) agrees with .comb on the same frugal pattern';

is-deeply 'aXbXc'.comb(/a.*?X/).List, ('aX',),
    'frugal .*? stops at the first X';
is-deeply 'aXbXc'.comb(/a.*X/).List, ('aXbX',),
    'greedy .* still runs to the last X';

is-deeply '<i>x</i><i>y</i>'.comb(/'<i>' .+? '</i>'/).List, ('<i>x</i>', '<i>y</i>'),
    'frugal .+? splits adjacent tags';
is-deeply '<i>x</i><i>y</i>'.comb(/'<i>' .+ '</i>'/).List, ('<i>x</i><i>y</i>',),
    'greedy .+ spans both tags';

is-deeply 'aaa'.comb(/a??/).List, ('', '', '', ''),
    'frugal ?? prefers zero matches';
is-deeply 'aaa'.comb(/a?/).List, ('a', 'a', 'a', ''),
    'greedy ? prefers one match';

is-deeply 'abcabc'.comb(/a .*? c/).List, ('abc', 'abc'),
    'frugal .*? between literals';
is-deeply 'foo=1;bar=2;'.comb(/ \w+? '=' \d /).List, ('foo=1', 'bar=2'),
    'frugal \w+? before a literal';

is-deeply 'aaab'.comb(/a**?1..3 b/).List, ('aaab',),
    'frugal ** range still grows to satisfy what follows';

is-deeply $s.split(/ \" .*? \" /).List, ('a ', ' c ', ''),
    '.split honours the frugal quantifier';
is $s.subst(/ \" .*? \" /, 'Q', :g), 'a Q c Q',
    '.subst(:g) honours the frugal quantifier';

is ('aXbXc' ~~ / <?before a .*? X > (.) /).Str, 'a',
    'a frugal quantifier inside a lookahead assertion still matches';

# vim: expandtab shiftwidth=4
