use Test;

plan 7;

# Raku's Str is NFG, so a literal written in the source is normalized when the
# program is compiled. mutsu already normalized the buffer around an *escape*
# (`\x[2126]`, `\x[0041,0300]`) and, since
# news/2026-08/decoded-strings-are-nfc.md, `.decode` output — but raw non-ASCII
# text in the source went through untouched, so a file containing U+2126 OHM
# SIGN produced a string that did not `eq` "\x[03A9]".
#
# The literals below are written with U+2126 on purpose; do not "tidy" them.

is 'Ω'.encode('utf-8').elems, 2, 'a single-quoted literal is NFC-normalized';
ok 'Ω' eq "\x[03A9]", 'and equals the composed form';
is "Ω".encode('utf-8').elems, 2, 'an interpolating literal too';
is q{Ω}.encode('utf-8').elems, 2, 'a q{} literal too';
is qq{Ω}.encode('utf-8').elems, 2, 'a qq{} literal too';

# Decomposed sequences written literally compose as well.
is "e\c[COMBINING ACUTE ACCENT]".chars, 1, 'an escape-built decomposed pair still composes';

# A hash key written with the un-normalized form finds the composed key.
my %h;
%h{"\x[03A9]"} = 'v';
is %h{'Ω'}, 'v', 'an un-normalized literal key finds the composed entry';
