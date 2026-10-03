use Test;

# A character class tests a whole grapheme. A grapheme of several codepoints
# (`x` + U+0301 COMBINING ACUTE ACCENT) equals no enumerated character or
# range, so a negated enumerated class matches it, while the class's
# backslash/property items (`\w`, `\s`, `<:L>`) still test its base
# character. Every expectation below was checked against rakudo (#10748).

plan 18;

my $s = "x\x[301]";
is $s.chars, 1, 'x + combining acute is one grapheme';

ok  $s ~~ /^ <-[a..z0..9\s]>+ $/, 'negated class with a backslash item, quantified';
ok  $s ~~ /^ <-[a..z]> $/, 'negated range';
ok  $s ~~ /^ <-[x]> $/, 'negated single character';
ok  $s ~~ /^ <-[0..9]> $/, 'negated range the base is not in';
nok $s ~~ /^ <[a..z\s]> $/, 'a positive range does not match the cluster';
ok  $s ~~ /^ <[\w]> $/, '\w tests the base character';
nok $s ~~ /^ <-[\w]> $/, 'a negated \w tests the base character too';
nok $s ~~ /^ <-[\w] + [x]> $/, 'composite: a positive character does not match the cluster';
nok $s ~~ /^ <[a..z] - [x]> $/, 'composite: a positive range does not match the cluster';
ok  $s ~~ / <-[a..z] - [\d]> /, 'composite: a negated range does not reject the cluster';

ok "ax\x[301]b" ~~ / <-[a..z]> /, 'an unanchored scan finds the cluster';
is ("ax\x[301]b" ~~ / <-[a..z]> /).Str.chars, 1, '... and consumes the whole grapheme';
is ~("x\x[301]y" ~~ / <-[a..z\s]>+ /), "x\x[301]", 'a quantified class stops at the next plain letter';

grammar G { token TOP { <-[a..z0..9\s]>+ } }
ok G.parse($s), 'the same class in a grammar token';

# `\r\n` is one grapheme of two ASCII codepoints: the scan prefilter must
# still offer the position of its `\r` to the engine (#11145).
is ("a\r\nb" ~~ / <-[a..z\r]> /).Str.ords, (13, 10), 'a negated class without \n matches the CRLF cluster';
nok "a\r\nb" ~~ / <-[a..z\r\n]> /, 'a negated class with both \r and \n rejects it';
is ~("ab\x[301]cd" ~~ / <-[a..c]> /), "b\x[301]", 'an ASCII base before a mark is offered to the engine';
