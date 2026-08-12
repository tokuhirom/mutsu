use v6;
use Test;

# An interpolated scalar in a regex matches its value LITERALLY — raku does
# not re-parse the value as regex source. The escape helper used to carry an
# enumerated metachar list and leaked whichever char it forgot: `~` (the
# goal-match marker) survived to the structural parser and PANICKED the
# matcher (Text::CSV 55_combi with `~` as quote/sep/escape char). Now every
# non-identifier char is escaped.

plan 14;

# The 55_combi shape: tilde as an alternation branch
my $e = "~";
ok "a~b" ~~ m/ $e /, 'interpolated "~" matches literally';
is $/.Str, "~", 'match is the tilde';

my $q = Q/"/;
my $t = "a~b";
$t ~~ s:g/( $q | $e )/X$0/;
is $t, "aX~b", 'tilde in alternation with capture substitutes correctly';

# Tilde between other atoms must not become the goal-match rewrite.
# (A QUANTIFIED interpolation `$sep?` is a separate pre-existing bug —
# todo/tickets/quantified-scalar-regex-interpolation-broken.md.)
my $sep = "~";
ok "(~y)" ~~ m/ "(" $sep /, 'tilde after another atom stays literal';

# Every historical leak candidate, one by one
for < ! = , ; ` / - > -> $ch {
    my $v = $ch;
    ok $v ~~ m/^ $v $/, "interpolated {$v.raku} matches itself";
}

# Alphanumerics must stay bare (escaping them would create class shorthands)
my $d = "d";
nok "5" ~~ m/ $d /, 'interpolated "d" does not become \\d';
ok "d" ~~ m/ $d /, 'interpolated "d" matches literal d';

# A Regex-valued scalar still interpolates as a regex, not a literal
my $rx = rx/\d+/;
ok "a42" ~~ m/ $rx /, 'Regex-valued scalar interpolates as regex';
