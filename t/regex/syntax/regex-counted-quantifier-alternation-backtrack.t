use Test;

# A counted quantifier over an alternation must be able to backtrack into an
# earlier iteration and take another branch there, as `*` and `+` already
# could: `[ a || aa ] ** 2` matches "aaa" by taking `aa` in the second
# iteration. The tree walk grew a `**` chain from each iteration's first
# branch only, so these failed. Expected values read off rakudo 2026.07.

plan 5;

is ("aaa" ~~ / ^ [ a || aa ] ** 2 $ /).Str, 'aaa', '|| under ** 2 backtracks into an iteration';
is ("abab" ~~ / ^ [ a || ab ] ** 2..3 b $ /).Str, 'abab', '|| under ** 2..3';
is ("aaa" ~~ / ^ [ a || aa ] ** 2..2 $ /).Str, 'aaa', '|| under ** 2..2';
is ("aaa" ~~ / ^ [ a || aa ] **? 1..2 $ /).Str, 'aaa', 'frugal ** over ||';
nok "aaa" ~~ / :r ^ [ a || aa ] ** 2 $ /, 'ratchet still commits to the first branch';
