use Test;

# #10403: the `<.ws>` that `:sigspace` inserts runs as a compiled regex op.
# These pin its semantics (`<!ww> \s*`, committed to the longest run).

plan 12;

ok  "a b"   ~~ / :s a b /, 'space between word chars matches';
ok  "a \t b" ~~ / :s a b /, 'a run of mixed whitespace matches';
nok "ab"    ~~ / :s a b /, 'no space between two word chars fails';
ok  "a,b"   ~~ / :s a ',' b /, 'no space needed next to a non-word char';
is ("a , b" ~~ / :s a ',' b /).Str, 'a , b', 'spaces around punctuation are consumed';
is ("a b  " ~~ / :s a b /).Str, 'a b  ', 'whitespace before the closing / is a <.ws> too';
ok  "a b  " ~~ / :s ^ a b $ /, 'a <.ws> before $ swallows trailing whitespace';
nok "a b  x" ~~ / :s ^ a b $ /, '... but not trailing text';
is ("a, a, a" ~~ / :s a+ % "," /).Str, 'a, a, a', 'separated quantifier under :s';
is ("x y z" ~~ / :s [ \w ]+ /).Str, 'x y z', 'a quantified group repeats its <.ws>';
nok "a  b" ~~ / :s a \s b /, '<.ws> does not give whitespace back';
is ~("foo  bar" ~~ / :s (\w+) (\w+) /)[1], 'bar', 'captures around <.ws>';
