use Test;

plan 16;

# Positive lookahead: <?before pattern>
ok "foobar" ~~ / foo <?before bar> /, "positive lookahead matches";
is ~$/, "foo", "positive lookahead captures only the non-assertion part";
nok "foobaz" ~~ / foo <?before bar> /, "positive lookahead fails when pattern doesn't follow";

# Negative lookahead: <!before pattern>
ok "foobaz" ~~ / foo <!before bar> /, "negative lookahead matches when pattern doesn't follow";
is ~$/, "foo", "negative lookahead captures only the non-assertion part";
nok "foobar" ~~ / foo <!before bar> /, "negative lookahead fails when pattern follows";

# Positive lookbehind: <?after pattern>
ok "foobar" ~~ / <?after foo> bar /, "positive lookbehind matches";
is ~$/, "bar", "positive lookbehind captures only the non-assertion part";
nok "bazbar" ~~ / <?after foo> bar /, "positive lookbehind fails when pattern doesn't precede";

# Negative lookbehind: <!after pattern>
ok "bazbar" ~~ / <!after foo> bar /, "negative lookbehind matches when pattern doesn't precede";
is ~$/, "bar", "negative lookbehind captures only the non-assertion part";
nok "foobar" ~~ / <!after foo> bar /, "negative lookbehind fails when pattern precedes";

# Combined assertions
ok "foobar" ~~ / <!after baz> <!before xxx> bar /, "combined negative lookbehind and lookahead";
is ~$/, "bar", "combined assertions capture correctly";

# Lookahead at end of string
nok "foo" ~~ / foo <?before bar> /, "positive lookahead at end of string fails";
ok "foo" ~~ / foo <!before bar> /, "negative lookahead at end of string succeeds";
