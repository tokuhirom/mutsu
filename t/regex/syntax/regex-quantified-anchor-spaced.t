use Test;

plan 9;

# A zero-width anchor followed by whitespace and then a quantifier takes the
# quantifier (the repetition is zero-width), as in rakudo. An adjacent
# quantifier (`^^+`) stays X::Syntax::Regex::NonQuantifiable (roast
# S05-metachars/line-anchors.t).
# https://github.com/tokuhirom/mutsu/issues/11873

is ("a b" ~~ / ^^ ** 2 a /).Str, 'a', '^^ ** 2 then a literal';
is ("a b" ~~ / ^^ ** 0..2 a /).Str, 'a', '^^ ** 0..2 then a literal';
is ("a b" ~~ / ^^ ? a /).Str, 'a', '^^ ? then a literal';
is ("a b" ~~ / ^^ * a /).Str, 'a', '^^ * then a literal';
is ("a b" ~~ / $$ ** 2 /).Str, '', '$$ ** 2';
is ("a b" ~~ / $ ** 2 /).Str, '', '$ ** 2';
nok ("a b" ~~ / b ^^ ** 2 /).Bool, 'a quantified ^^ still asserts line start';

throws-like q[/ ^^+ /], X::Syntax::Regex::NonQuantifiable, 'adjacent quantifier on ^^';
throws-like q[/ $$+ /], X::Syntax::Regex::NonQuantifiable, 'adjacent quantifier on $$';
