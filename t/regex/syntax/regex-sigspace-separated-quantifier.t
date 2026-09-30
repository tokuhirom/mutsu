use v6;
use Test;

# Under `:sigspace`, a separated quantifier (`atom+ % sep`) is parsed
# natively, with the whitespace around it placed the way Rakudo places it:
#   * after the separator atom  -> a `<.ws>` inside the separator;
#   * between quantifier and %  -> a `<.ws>` after the whole quantifier;
#   * between atom and quantifier -> a `<.ws>` after the atom, per item.
# It used to be expanded to text, which lost a frugal quantifier's priority
# and leaked `ws` captures into the Match (#10339).

plan 22;

# Frugal separated quantifiers keep their priority.
is ~("a, a, a" ~~ / :s a*? % "," /), '', '*? % under :s matches zero items first';
is ~("a, a, a" ~~ / :s a**?2..3 % "," /), 'a, a', '**?2..3 % under :s takes the minimum';
is ~("a, a, a" ~~ / :s a+? % "," /), 'a', '+? % under :s takes one item';
is ~("a, a, a," ~~ / :s a+? %% "," $ /), 'a, a, a,', '+? %% under :s grows to reach an anchor';

# Greedy forms and the whitespace placement.
is ~("a, a, a" ~~ / :s a* % "," /), 'a, a, a', '* % "," takes every item';
is ~("a, a, a" ~~ / :s a**2..3 % "," /), 'a, a, a', '**2..3 % "," takes the maximum';
is ~("a , a , a" ~~ / :s a+ % "," /), 'a ', 'no <.ws> before the separator; one after the quantifier';
is ~("a , a , a" ~~ / :s a+% "," /), 'a', 'no whitespace before % means no trailing <.ws>';
is ~("a, a, a" ~~ / :s a+ % ","/), 'a', 'no whitespace after the separator means no <.ws> in it';
is ~("a,a,a" ~~ / :s a+ % ","/), 'a,a,a', 'an unspaced subject still matches';
is ~("a , a , a" ~~ / :s a +% "," /), 'a , a , a', 'whitespace before the quantifier is a per-item <.ws>';
ok "a , b ,c" ~~ /:s^ <alpha> +% \, $/, 'per-item <.ws> with a builtin subrule';
nok "a,b" ~~ / :s a+ %"," b /, 'the separator\'s <.ws> is not matched before the following atom';
is ~("a, a, a x" ~~ / :s a+ %% "," x /), 'a, a, a x', '%% under :s without a trailing separator';
is ~("a, a, a, x" ~~ / :s a+ %% "," x /), 'a, a, a, x', '%% under :s with a trailing separator';
is ~("a  ,  a" ~~ / :s a+ % [ "," ] /), 'a  ', 'a group separator keeps its inner sigspace';

# No `ws` captures leak into the Match.
is ("a, a, a" ~~ / :s a+ % "," /).hash.elems, 0, 'sigspace separators capture nothing';

# A capture name on a separated token names each item, sigspace or not.
{
    my $m = "1,2" ~~ / <digit>+ % "," /;
    is $m<digit>.elems, 2, '<digit>+ % "," captures one Match per item';
    is ~$m<digit>[1], '2', '... the second item is its own Match';
    $m = "1, 2" ~~ / :s <digit>+ % "," /;
    is $m<digit>.map(~*).join('|'), '1|2', 'the same under :s';
    $m = "1, 2" ~~ / :s $<x>=\d ** 2 % "," /;
    is ~$m<x>, '1, 2', 'a sigil alias still captures the whole separated span';
    $m = "1,2" ~~ / $<x>=\d+ % "," /;
    is ~$m<x>, '1,2', '... also without :s';
}
