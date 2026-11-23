use Test;

# #18967: a `**` range takes its backtracking modifier (`?`, `!`, `:?`, `:!`)
# before the bounds, and blanks may surround it: `a ** :? 2..3`.

plan 8;

is ~("aaaa" ~~ / a ** :? 2..3 /), 'aa', 'spaced colon-frugal range';
is ~("aaaa" ~~ / a ** :! 2..3 /), 'aaa', 'spaced colon-greedy range';
is ~("aaaa" ~~ / a ** ?2..3 /), 'aa', 'spaced frugal range';
is ~("aaaa" ~~ / a ** ! 2..3 /), 'aaa', 'spaced greedy range';
is ~("aaa" ~~ / a ** :? 2 /), 'aa', 'spaced frugal count';
is ~("abab" ~~ / [ab] ** :? 1..2 /), 'ab', 'a group with a spaced frugal range';
is ~("aaaa" ~~ / a**:?2..3 /), 'aa', 'the unspaced form still matches';
is ~("a 4" ~~ rx:s/a ** 1 4/), 'a 4', 'a spaced range under :sigspace';
