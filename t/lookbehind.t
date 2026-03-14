use Test;

plan 10;

ok "foobar" ~~ / <?after foo> bar /, "positive lookbehind matches";
is ~$/, "bar", "positive lookbehind captures only the non-assertion part";
nok "bazbar" ~~ / <?after foo> bar /, "positive lookbehind fails when pattern doesn't precede";

ok "foobar" ~~ / <after foo> bar /, "bare positive lookbehind matches";
nok "bazbar" ~~ / <after foo> bar /, "bare positive lookbehind fails when pattern doesn't precede";

ok "bazbar" ~~ / <!after foo> bar /, "negative lookbehind matches when pattern doesn't precede";
is ~$/, "bar", "negative lookbehind captures only the non-assertion part";
nok "foobar" ~~ / <!after foo> bar /, "negative lookbehind fails when pattern precedes";

ok "bazbar" ~~ / <after ba<[z]>> bar /, "nested bare positive lookbehind matches";
nok "bacbar" ~~ / <after ba<[z]>> bar /, "nested bare positive lookbehind fails when nested assertion does not match";
