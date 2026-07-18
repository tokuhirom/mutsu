use v6;
use Test;

plan 6;

# `** 0` is a valid "exactly zero times" quantifier (RFC 3986's
# `token path-empty { <.pchar> ** 0 }`).
grammar G {
    token empty { <.c> ** 0 }
    token c { \w }
}

ok G.parse('', rule => 'empty'), 'token with ** 0 matches the empty string';
nok G.parse('x', rule => 'empty'), '** 0 does not consume characters';

my regex wrap { [ $<pe> = <G::empty> ] }
ok '' ~~ /^ <wrap> $/, '** 0 subrule matches via an aliasing wrapper';

ok 'ab' ~~ /a ** 0 b/, 'a ** 0 b matches the b (zero a-s)';
nok 'aab' ~~ /^ a ** 0 $/, 'anchored ** 0 fails when characters remain';
ok 'aab' ~~ /^ a ** 2 b $/, 'plain ** N still works';
