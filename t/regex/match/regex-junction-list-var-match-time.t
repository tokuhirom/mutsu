use Test;

# A junction or list held in a `$` variable is read when the cursor reaches it
# (#14510): the junction is an alternation of its eigenstates, a list matches
# as its string. Expectations measured against rakudo.

plan 10;

my $j = any("ab", "cd");
is ~("zcd" ~~ / z $j /), 'zcd', 'a junction interpolates as an alternation';
is ~("ab" ~~ / ^ $j $ /), 'ab', 'the first eigenstate matches';
nok "ax" ~~ / ^ $j $ /, 'no eigenstate matches';
is ~("xCDx" ~~ / :i $j /), 'CD', 'a junction under :i';

# A code block rewrites the variable before the atom is reached.
my $v = "a";
is ~("xb" ~~ / x { $v = any("b", "c") } $v /), 'xb', 'a junction assigned by a block is read late';

my $l = <ab cd>;
is ~("ab cd" ~~ / $l /), 'ab cd', 'a list matches as its string';
nok "cd" ~~ / ^ $l $ /, 'and not as an alternation';

my $m = any("a", "b");
is ~("ab" ~~ / $m+ /), 'ab', 'a quantified junction atom';
is ~("zb" ~~ / z [ $m ] /), 'zb', 'inside a group';

my $n = "q";
is ~("xqy" ~~ / x $n y /), 'xqy', 'a plain string is unchanged';
