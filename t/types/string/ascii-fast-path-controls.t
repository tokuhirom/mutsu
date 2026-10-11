use Test;

# The compiled regex engine answers a one-grapheme atom from an ASCII set at a
# position where the grapheme is provably one character. TAB and LF are covered
# too, except an LF after CR (CR LF is a single grapheme). Each expectation was
# checked against rakudo.

plan 14;

my $crlf = "ab\r\ncd";
ok $crlf ~~ /b \n c/, 'CR LF is matched by \n';
nok $crlf ~~ /b \r c/, '... and not by \r alone';
ok "a\nb" ~~ /a \n b/, 'a lone LF is matched by \n';
ok "a\tb" ~~ /a \t b/, 'a TAB is matched by \t';
ok "a\tb" ~~ /a \s b/, '... and by \s';
ok "a\nb" ~~ /a \s b/, 'an LF is matched by \s';
ok "a\r\nb" ~~ /a \s b/, 'CR LF is one \s';
nok "a\r\nb" ~~ /a \s \s b/, '... not two';
ok "a\nb" ~~ /a <[\n]> b/, 'an LF is matched by a class';
nok "a\n\x[301]b" ~~ /a \n b/, 'an LF extended by a combining mark is not a bare LF';
ok "x\n" ~~ /\w+ \s/, 'a word run followed by a final newline';
ok "xy\n\nz" ~~ /\w+ \s+ z/, 'a word run, blank lines, a word';
is "a1 b22\nc333".match(/ \w+ \s* /, :g).elems, 3, 'a global match across a newline';
is ("one\ntwo\n" ~~ / \w+ \n \w+ /).Str, "one\ntwo", 'a match spanning a newline';
