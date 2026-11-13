use Test;

# A bare sigil inside a double-quoted regex atom ("@", "$", "%", "&") is
# literal text, not an interpolation; a sigil followed by a name still
# interpolates. Cro::Uri's `authority` token is the shape: `"@"` beside
# subrules, matched once per URI.

plan 12;

ok "a@b" ~~ /"@"/, '"@" matches a literal at sign';
ok "x\$" ~~ /"\$"/, 'an escaped "\$" matches a dollar';
ok "%" ~~ /"%"/, '"%" matches a percent sign';
ok "&" ~~ /"&"/, '"&" matches an ampersand';
ok "e@x" ~~ /"e@" x/, 'a trailing bare "@" in a longer literal';

my $v = "q";
ok "aq" ~~ /"a$v"/, 'a named scalar still interpolates';
my @a = <z>;
ok "z" ~~ /"@a[]"/, 'an array with a postcircumfix still interpolates';
nok "a\$v" ~~ /^ "a$v" $/, 'and is not matched literally';

grammar Auth {
    token TOP { [ <user> "@" ]? <host> [ ":" <port> ]? }
    token user { \w+ }
    token host { <[a..z.]>+ }
    token port { \d+ }
}
is ~Auth.parse("me@ex.com:80")<user>, 'me', 'a grammar token with a quoted "@"';
is ~Auth.parse("me@ex.com:80")<port>, '80', 'and a quoted ":"';
nok Auth.parse("ex.org")<user>, 'the optional part may be absent';
is (^50).map({ ~Auth.parse("u$_\@h.org")<user> }).tail, 'u49', 'repeated parses stay correct';
