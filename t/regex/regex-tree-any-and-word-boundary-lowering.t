use Test;

# `.` and the `<<` / `>>` word boundaries lowered straight from the retained
# regex tree must match what the string-parsed spelling matched.
plan 24;

is ("abc" ~~ /a . c/).Str, 'abc', '. matches one char between literals';
nok "ac" ~~ /a . c/, '. needs a char';
is ("a\nc" ~~ /a . c/).Str, "a\nc", '. matches a newline';
is ("xyz" ~~ /^ . ** 3 $/).Str, 'xyz', '. under a ** quantifier';
is ("abc" ~~ /.+/).Str, 'abc', '.+ takes everything';
is ("abc" ~~ /.+?/).Str, 'a', 'frugal .+?';
is ("abcd" ~~ /(.) (.)/)[1].Str, 'b', 'captures around .';
is ("ab" ~~ /:ratchet .* b/).Bool, False, 'ratchet .* does not backtrack';
is ("é" ~~ /^ . $/).Str, 'é', '. is Unicode aware';

ok "foo bar" ~~ /<< foo/, '<< at start of a word';
nok "xfoo" ~~ /<< foo/, '<< rejects mid-word';
ok "foo bar" ~~ /foo >>/, '>> at end of a word';
nok "foox" ~~ /foo >>/, '>> rejects mid-word';
ok "foo bar" ~~ /<< foo >>/, 'both boundaries';
ok "foo" ~~ /<< foo >>/, 'boundaries at string edges';
nok "afoob" ~~ /<< foo >>/, 'boundaries reject embedded';
ok "foo" ~~ /« foo »/, 'French quotes are boundaries too';
is ("the cat sat" ~~ /<< c.t >>/).Str, 'cat', 'boundaries around .';
is "one two  three".comb(/<< \w+ >>/).join('|'), 'one|two|three', 'comb words';
is ("a-b" ~~ /a >> '-'/).Str, 'a-', '>> before punctuation';

my token t { <<  \w+ >> }
is ("hi there" ~~ /<t>/).Str, 'hi', 'boundary in a token';
my regex r { . . }
is ("abc" ~~ /<r>/).Str, 'ab', '. in a named regex';
is ("a.b" ~~ /a \. b/).Str, 'a.b', 'escaped dot is a literal';
is ("axb" ~~ /a \. b/).Bool, False, 'escaped dot is not any';
