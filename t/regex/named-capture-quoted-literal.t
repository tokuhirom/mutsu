use Test;

plan 9;

# `$<k>='abc'` aliases the whole quoted literal, not its first character.
is ("run x" ~~ /$<key>='run' \s/)<key>, "run", "single-quoted literal alias";
is ("abc" ~~ /$<k>="abc"/)<k>, "abc", "double-quoted literal alias";
is ("xrun" ~~ /x $<k>='run'/)<k>, "run", "alias after another atom";
is ("javascript" ~~ /$<k>=[ 'javascript' | 'bash' ]/)<k>, "javascript", "alias on an alternation group";
is ("ab" ~~ /$<k>='ab'/).Str, "ab", "the whole match";
is ("ab" ~~ /$<k>=['ab']/)<k>, "ab", "bracketed form unchanged";
is ("ab" ~~ /$<k>=("ab")/)<k>, "ab", "paren form unchanged";
is ("run" ~~ /$<key>=run/)<key>, "r", "a bare word aliases only its first character, as in Rakudo";
grammar G { token TOP { $<key>='run' $<rest>=.* } }
is G.parse("run /tmp/x")<key>, "run", "inside a grammar token";
