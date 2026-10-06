use Test;

plan 22;

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

# A leading `^` is no token of its own (it anchors the pattern), so a spaced
# quantifier on it used to have nothing to attach to (#12039). It repeats a
# zero-width assertion: a minimum of 1 or more is still start-of-string, and a
# minimum of 0 makes the assertion optional.
is ("a b" ~~ / ^ ** 2 a /).Str, 'a', '^ ** 2 then a literal';
nok ("b a" ~~ / ^ ** 2 a /).Bool, 'a quantified ^ still asserts start of string';
is ("a b" ~~ / ^ ** 1..3 a /).Str, 'a', '^ ** 1..3 then a literal';
nok ("b a" ~~ / ^ ** 1..3 a /).Bool, '^ ** 1..3 still asserts start of string';
is ("b a" ~~ / ^ ** 0..2 a /).Str, 'a', '^ ** 0..2 makes the anchor optional';
is ("b a" ~~ / ^ ** 0 a /).Str, 'a', '^ ** 0 makes the anchor optional';
is ("b a" ~~ / ^ ? a /).Str, 'a', '^ ? makes the anchor optional';
is ("b a" ~~ / ^ ?? a /).Str, 'a', '^ ?? makes the anchor optional';
is ("a b" ~~ / ^ ** 2 $ /).Bool, False, '^ ** 2 combined with a trailing $';

# An adjacent quantifier on `^` stays NonQuantifiable, as before.
throws-like q[/ ^+ /], X::Syntax::Regex::NonQuantifiable, 'adjacent + on ^';
throws-like q[/ ^? /], X::Syntax::Regex::NonQuantifiable, 'adjacent ? on ^';
throws-like q[/ ^* /], X::Syntax::Regex::NonQuantifiable, 'adjacent * on ^';
throws-like q[/ ^** 2 /], X::Syntax::Regex::NonQuantifiable, 'adjacent ** on ^';
