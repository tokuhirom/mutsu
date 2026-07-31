use v6;
use Test;

plan 6;

# A backslash-escaped character inside a [...] group must not affect the
# group's bracket depth: `[<?[\]]>||$]` closes at the final `]`, not at the
# char class's escaped `\]` (Cro::Uri's IPv4address rule).
ok "x" ~~ /x [<?[\]]> || $]/, 'lookahead class with escaped ] inside a group, end-anchored branch';
nok "x/" ~~ /^x [<?[\]]> || $]/, 'the group really is a lookahead-or-end, not a literal';
ok "x]" ~~ /x [<?[\]]> || $] ./, 'the lookahead branch matches before ]';

# The Cro::Uri IPv4address shape: separated quantifier + lookahead-or-end group.
my regex dec-octet {
    | <[0..9]>
    | <[1..9]> <[0..9]>
    | "1" <[0..9]> <[0..9]>
    | "2" <[0..4]> <[0..9]>
    | "25" <[0..5]>
}
my regex ipv4 { <.dec-octet> ** 4 % "." [<?[/#?:\]]> || $] }
ok "127.0.0.1" ~~ /^<ipv4>$/, 'IPv4address matches at end of string';
ok "127.0.0.1:80" ~~ /^<ipv4>/, 'IPv4address matches before a colon';
nok "127.0.0.1x" ~~ /^<ipv4>/, 'IPv4address rejects a trailing word char';
