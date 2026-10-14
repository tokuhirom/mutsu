# `\0` in a regex is the NUL character, outside a char class as well as
# inside one. Found in Crypt::SodiumPasswordHash, which strips the NUL
# padding of a C buffer with `.subst(/\0+$/, '')`.
use Test;

plan 6;

is "AB\0\0".subst(/\0+$/, ''), 'AB', 'trailing NULs stripped with /\0+$/';
ok "A\0B" ~~ /\0/, '\0 matches a NUL';
nok "A0B" ~~ /\0/, '\0 does not match the digit 0';
is ~("\x[0]0" ~~ /\00/), "\x[0]0", 'a digit after \0 is a literal';
is "a\0b".split(/\0/).join(','), 'a,b', 'split on /\0/';
is ~("a\0b" ~~ /<[\0]>/), "\0", '\0 inside a char class still matches NUL';
