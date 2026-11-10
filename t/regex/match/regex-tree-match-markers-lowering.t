use Test;

# `<(` / `)>` and `<?>` lowered straight from the retained regex tree must
# behave as the string-parsed spelling did.
plan 12;

is ("foobar" ~~ / foo <( bar /).Str, 'bar', '<( starts the match';
is ("foobar" ~~ / foo <( bar /).from, 3, '<( moves .from';
is ("foobar" ~~ / <( foo )> bar /).Str, 'foo', ')> ends the match';
is ("foobar" ~~ / <( foo )> bar /).to, 3, ')> moves .to';
is ("foobar" ~~ / f <( oo )> bar /).Str, 'oo', 'both markers';
is ("x=42" ~~ / '=' <( \d+ /).Str, '42', 'marker before a quantified class';
is ("abc" ~~ / a <?> b /).Str, 'ab', '<?> matches nothing and passes';
ok "" ~~ / <?> /, '<?> alone matches the empty string';
is ("ab" ~~ / <?> ab /).Str, 'ab', '<?> before a literal';
is ("abc" ~~ / (a) <( (b) /).Str, 'b', 'marker between captures';
is ("abc" ~~ / (a) <( (b) /)[1].Str, 'b', 'the capture after the marker';
my token t { a <( b }
is ("ab" ~~ /<t>/).Str, 'ab', 'a marker inside a token does not move the outer match';
