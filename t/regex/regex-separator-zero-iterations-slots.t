use Test;

# A separated quantifier that matches zero iterations still reserves the
# positional slots of its atom's (and separator's) capture groups, as empty
# lists, exactly like the unseparated quantifier. Expected values checked
# against rakudo (#10534).

plan 12;

"" ~~ / [ (\d) ] *% ',' /;
is-deeply $/.list, ([],), 'bracketed group, *%';
"" ~~ / (\d)* % ',' /;
is-deeply $/.list, ([],), 'bare group, * %';
"" ~~ / [ (\d) ]* /;
is-deeply $/.list, ([],), 'control: unseparated';
"x" ~~ / (\d)* % (',') x /;
is-deeply $/.list, ([], []), 'a separator capture group gets its slot too';
"" ~~ / [(\d) (\w)]* % ',' /;
is-deeply $/.list, ([], []), 'two groups in the atom';
"" ~~ / (\d)** 0..3 % ',' /;
is-deeply $/.list, ([],), 'a ** 0..3 range';
"" ~~ / :r (\d)* % ',' /;
is-deeply $/.list, ([],), 'ratchet';
"" ~~ / (\d)*? % ',' /;
is-deeply $/.list, ([],), 'frugal';
"" ~~ / (\d)* %% ',' /;
is-deeply $/.list, ([],), '%% form';

"a" ~~ / (\d)* % ',' (\w) /;
is ~$1, 'a', 'a later group still lands in the next slot';
is $0.elems, 0, 'the zero-iteration slot is an empty list';

"1,2" ~~ / (\d)* % ',' /;
is $0.elems, 2, 'non-empty iterations are unaffected';
