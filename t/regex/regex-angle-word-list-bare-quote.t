use v6;
use Test;

# A `< a b c >` alternation inside a regex lists literal words, and a lone `'`
# (or `"`) there is an ordinary one-character word — not a quote delimiter. Two
# scanners used to toggle quote state on it and then treat the rest of the
# pattern as quoted, which silently ate the assertion's closing `>` (dropping its
# quantifier) and suppressed top-level `|` / `||` splitting.
#
# This is the shape of the HTTP `tchar` production, used by HTTP::MediaType:
#     token tchar { || < ! # $ % & ' * + - . ^ _ ` | ~ > || <.DIGIT> || <.ALPHA> }

plan 14;

# The lone quote is itself matchable as a word.
ok "'" ~~ / ^ < ! ' # > $ /, "a lone ' in the word list matches as a word";
ok '"' ~~ / ^ < ! " # > $ /, 'a lone " in the word list matches as a word';
ok "!" ~~ / ^ < ! ' # > $ /, "the other words still match";

# The quantifier after `>` is not swallowed.
is ("!#" ~~ / ^ < ! ' # >+ /).pos, 2, "a `+` after a quote-containing word list repeats";
is ("!!" ~~ / ^ < ! ' # >+ /).pos, 2, "... independently of which word matched";
is ("''" ~~ / ^ < ! ' # >+ /).pos, 2, "... including repeats of the quote word";
is ("!#" ~~ / ^ < ! ' # >* /).pos, 2, "a `*` after a quote-containing word list repeats";
is ("!#" ~~ / ^ < ! # >+ /).pos, 2, "a list without a quote is unaffected";

# A following alternation branch still splits.
ok "t" ~~ / ^ [ < ! ' # > || <[A..Za..z]> ] $ /, "`||` after the word list still splits";
ok "t" ~~ / ^ [ < ! ' # > | <[A..Za..z]> ] $ /, "`|` after the word list still splits";
ok "!" ~~ / ^ [ < ! ' # > || < a b > ] $ /, "two word lists in an alternation";

# Genuine quoted literals elsewhere in the pattern keep working.
ok "!x" ~~ / ^ < ! ' # > 'x' $ /, "a real quoted literal after the list still parses";
ok "ab" ~~ / ^ 'a' || 'b' $ /, "top-level `||` between quoted literals still splits";

# An apostrophe inside an identifier-like word is still part of that word.
is ("don't" ~~ / ^ < don't yes >+ /).pos, 5, "an apostrophe inside a word stays in the word";

done-testing;
