use Test;

# Positional Str methods resolve grapheme positions through a cached per-string
# index (#9140). These pin the answers on strings long enough to be cached, with
# multi-codepoint graphemes (combining mark, CRLF, ZWJ emoji) spread through
# them, and check that a changed string never reuses a stale index.

plan 30;

# 6 graphemes per unit: a, e-acute, CRLF, q + combining acute, あ, ZWJ family.
my $unit = "a\x[E9]\r\nq\x[301]\x[3042]\x[1F468]\x[200D]\x[1F469]\x[200D]\x[1F467]";
my $s = $unit x 60;

is $s.chars, 360, 'chars counts graphemes';
is chars($s), 360, 'chars() sub form';
is $s.substr(0, 1), 'a', 'substr at 0';
is $s.substr(2, 1), "\r\n", 'CRLF is one grapheme';
is $s.substr(3, 1), "q\x[301]", 'combining mark stays with its base';
is $s.substr(6 * 45 + 4, 2), "\x[3042]\x[1F468]\x[200D]\x[1F469]\x[200D]\x[1F467]",
    'substr deep into the string';
is $s.substr(357), "q\x[301]\x[3042]\x[1F468]\x[200D]\x[1F469]\x[200D]\x[1F467]",
    'substr to the end';
is $s.substr(*-2, 1), "\x[3042]", 'substr with a WhateverCode start';
is $s.substr(98..99), "\r\nq\x[301]", 'substr with a Range';
is substr($s, 247, 1), "\x[E9]", 'substr() sub form';

is $s.index("\x[3042]", 200), 202, 'index from a position';
is $s.index("q"), Nil, 'index does not match a base inside a grapheme';
is $s.rindex("\x[3042]"), 358, 'rindex without a position';
is $s.rindex("\x[3042]", 99), 94, 'rindex from a position';
is $s.rindex("a", 5), 0, 'rindex back to the start';
is $s.rindex("q"), Nil, 'rindex does not match a base inside a grapheme';
is $s.indices("\r\n").elems, 60, 'indices finds every CRLF';
is $s.indices("\r\n")[59], 356, 'indices reports grapheme positions';

ok $s.contains("\x[3042]", 358), 'contains from a position';
nok $s.contains("\x[3042]", 359), 'contains past the last hit';
ok $s.substr-eq("\x[3042]", 184), 'substr-eq at a grapheme position';
nok $s.substr-eq("\x[3042]", 185), 'substr-eq elsewhere';
ok "q\x[301]a".substr-eq("a", 1), 'substr-eq counts graphemes, not codepoints';
ok $s.starts-with("a\x[E9]"), 'starts-with';
ok $s.ends-with("\x[3042]\x[1F468]\x[200D]\x[1F469]\x[200D]\x[1F467]"), 'ends-with';
is $s.ord, 97, 'ord';

# A string that changes must not answer from the old string's index.
my $t = "\x[3042]" x 300;
is $t.chars, 300, 'chars before append';
$t ~= "a\x[301]b";
is $t.chars, 302, 'chars after append';
is $t.substr(300, 2), "a\x[301]b", 'substr after append';
$t = "x" x 400;
is $t.index("x", 399), 399, 'index after reassignment to a flat string';
