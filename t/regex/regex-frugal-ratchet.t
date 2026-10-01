# A frugal quantifier under ratchet (`:r`, a `token`/`rule`) still grows on
# demand, shortest first, until the rest of the pattern matches; ratchet only
# commits each iteration's atom (and separator) to its first match. That holds
# for `*?`, `+?`, `**?`, `??` and the separated forms. Values verified against
# rakudo. The compiled regex engine (ADR-0135) used to decline these patterns
# (`frugal-ratchet`), and the walk answered `??` and `% sep` wrongly.
use Test;

plan 16;

is ~("aaab" ~~ /:r a*? b/), 'aaab', '*? grows until the rest matches';
is ~("aaab" ~~ /:r a*? /), '', '*? alone takes zero';
is ~("aaa" ~~ /:r a+? /), 'a', '+? alone takes one';
is ~("aaab" ~~ /:r a+? ab/), 'aaab', '+? gives the next atom its turn';
is ("aab" ~~ /:r (a)*? b/)[0].elems, 2, 'a captured *? folds every iteration';
is ~("xaab" ~~ /:r x [a|aa]*? b/), 'xaab', 'each iteration commits to its first branch';
is ~("aab" ~~ /:r a **? 1..3 b/), 'aab', '**? grows within its range';

is ~("ab" ~~ /:r a?? ab/), 'ab', '?? tries zero first';
is ~("ab" ~~ /:r a?? b/), 'ab', '?? then tries one';
is ~("ab" ~~ /:r [a||x]?? ab/), 'ab', '?? over an ordered alternation tries zero first';

is ~("a,a,ab" ~~ /:r a+? % "," b/), 'a,a,ab', '+? % grows';
is ~("a,a,a" ~~ /:r ^ a+? % "," $/), 'a,a,a', '+? % grows to the anchor';
is ~("a,a,ab" ~~ /:r a **? 2..5 % "," b/), 'a,a,ab', '**? % grows within its range';
is ~("a,a,a," ~~ /:r ^ a+? %% "," $/), 'a,a,a,', '+? %% takes its trailing separator';
is ~("b" ~~ /:r a*? % "," b/), 'b', '*? % takes zero first';

grammar G { token TOP { <w>+? "!" }; token w { \w } }
is G.parse("abc!")<w>.elems, 3, 'a token grows a frugal subrule loop';
