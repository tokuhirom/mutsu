use Test;

plan 8;

# A `** N % sep` separated quantifier must keep its separator when the
# quantified atom is NOT the first token of the pattern. The LTM string
# expansion used to mis-treat the whole prefix as the repeated atom, silently
# dropping the separator (Cro::HTTP's cookie-header test matches
# /"Cookie: " [...] ** 3 % '; '/).

ok "Zb; b" ~~ /Z "b" ** 2 % "; "/, 'literal prefix + ** N % sep matches';
nok "Zbb" ~~ /Z "b" ** 2 % "; "/, 'separator is enforced (no sep, no match)';
nok "Zb" ~~ /Z "b" ** 2 % "; "/, 'count is enforced';
ok "Zb; c; a" ~~ /Z ["a"||"b"||"c"] ** 3 % "; "/,
    'prefix + alternation group ** N % sep';
ok "X: b; c; a\r\n" ~~ /"X: " ["a"||"b"||"c"] ** 3 % "; " "\r\n"/,
    'prefix and suffix around ** N % sep';

# First-position forms unchanged.
ok "b; b" ~~ /"b" ** 2 % "; "/, 'first-position ** N % sep still matches';
nok "bb" ~~ /"b" ** 2 % "; "/, 'first-position separator still enforced';

# Capturing atom keeps working (native separated path).
my $m = "1.2.3.4" ~~ /(\d) ** 4 % "."/;
is $m[0].elems, 4, 'captures fold per iteration with prefix-free sep form';
