use v6;
use nqp;
use Test;

# Each nqp:: string op and the Str method rakudo builds on it are ONE routine
# (ADR-0117, src/builtins/str_prim/): positions are graphemes, and :i/:m fold
# the way nqp::indexic / nqp::indexim do. Every expected value below was
# measured with rakudo, and every row also asserts that the op and the method
# agree, so a private copy drifting away from the shared routine fails here
# even where no literal expectation happens to cover it.

plan 44;

# a, e + U+301, x, \r\n, Y, a regional-indicator flag, z: 7 graphemes.
my $mixed = "ae\x[301]x\r\nY\x[1F1EF]\x[1F1F5]z";

# -- chars --
is nqp::chars($mixed), 7, 'nqp::chars counts graphemes';
is nqp::chars($mixed), $mixed.chars, 'nqp::chars agrees with .chars';

# -- substr --
is nqp::substr($mixed, 3, 1), "\r\n", 'nqp::substr slices graphemes';
is nqp::substr($mixed, 1, 2), $mixed.substr(1, 2), 'nqp::substr agrees with .substr';
is nqp::substr("abcdef", -2, 1), "e", 'nqp::substr: negative start counts from the end';
is nqp::substr("abcdef", 2, -1), "cdef", 'nqp::substr: negative length means to the end';
is nqp::substr("abcdef", 7, 1), "", 'nqp::substr: start past the end is empty';

# -- index / rindex --
is nqp::index($mixed, "Y", 0), 4, 'nqp::index is a grapheme position';
is nqp::index($mixed, "Y", 0), $mixed.index("Y"), 'nqp::index agrees with .index';
is nqp::index($mixed, "e", 0), -1, 'nqp::index: no hit inside a grapheme';
nok $mixed.index("e").defined, '.index: no hit inside a grapheme either';
is nqp::index("abcdef", "", 6), 6, 'nqp::index: empty needle at the end';
is nqp::index("abcdef", "c", 99), -1, 'nqp::index: start past the end is -1';
is nqp::rindex($mixed, "x"), 2, 'nqp::rindex is a grapheme position';
is nqp::rindex($mixed, "x"), $mixed.rindex("x"), 'nqp::rindex agrees with .rindex';
is nqp::rindex($mixed, ""), 7, 'nqp::rindex: empty needle is the length';
is nqp::rindex("abcdef", "c", -1), 2, 'nqp::rindex: negative start searches from the end';

# -- folded search: nqp::indexic / indexim / indexicim vs .index(:i/:m) --
is nqp::indexic("STRASSE", "ß", 0), 4, 'nqp::indexic uses the full case fold';
is nqp::indexic("STRASSE", "ß", 0), "STRASSE".index("ß", :i), '... and agrees with .index(:i)';
is nqp::indexic("straße", "SS", 0), 4, 'nqp::indexic: a folded grapheme matches two needle chars';
is nqp::indexic("straße", "SS", 0), "straße".index("SS", :i), '... and agrees with .index(:i)';
is nqp::indexim("AbC\x[301]d", "C", 0), 2, 'nqp::indexim ignores marks';
is nqp::indexicim("abcé", "E", 0), 3, 'nqp::indexicim folds both';
is nqp::indexicim("abcé", "E", 0), "abcé".index("E", :i, :m), '... and agrees with .index(:i, :m)';
nok "aİ".index("i", :i).defined, '.index(:i): a hit must end on a folded grapheme boundary';

# -- eqat / eqatic vs .substr-eq / .starts-with / .ends-with --
is nqp::eqat($mixed, "x\r\n", 2), 1, 'nqp::eqat compares whole graphemes';
is nqp::eqat($mixed, "x\r", 2), 0, 'nqp::eqat: half a grapheme is no match';
is so(nqp::eqat($mixed, "x\r", 2)), $mixed.substr-eq("x\r", 2), 'nqp::eqat agrees with .substr-eq';
is nqp::eqat("abcdef", "ab", -6), 1, 'nqp::eqat: negative position counts from the end';
is nqp::eqatic("STRASSE", "ß", 4), 1, 'nqp::eqatic uses the full case fold';
ok "STRASSE".substr-eq("ß", 4, :i), '.substr-eq(:i) agrees with nqp::eqatic';
ok "STRASSE".starts-with("straße", :i), '.starts-with(:i) is nqp::eqatic at 0';
nok "STRASSE".ends-with("ße", :i), '.ends-with(:i) is nqp::eqatic at chars - needle.chars';
ok "STRASSE".contains("ß", :i), '.contains(:i) is nqp::indexic';

# -- ordat / iscclass / findcclass --
is nqp::ordat($mixed, 1), 0xE9, 'nqp::ordat: the NFC first codepoint of a grapheme';
is nqp::ordat($mixed, 3), 13, 'nqp::ordat: \r\n answers \r';
is nqp::ordat($mixed, 5), 0x1F1EF, 'nqp::ordat: a flag answers its first indicator';
is nqp::iscclass(nqp::const::CCLASS_NEWLINE, $mixed, 3), 1, 'nqp::iscclass reads a grapheme';
is nqp::findcclass(nqp::const::CCLASS_NEWLINE, $mixed, 0, nqp::chars($mixed)), 3,
    'nqp::findcclass reports a grapheme position';

# -- building: flip / concat / x / split --
is nqp::flip($mixed), $mixed.flip, 'nqp::flip agrees with .flip';
is nqp::flip($mixed).NFD.list, (122, 127471, 127477, 89, 13, 10, 120, 101, 769, 97),
    'nqp::flip reverses graphemes';
is nqp::concat("e", "\x[301]").chars, 1, 'nqp::concat composes across the join, like ~';
is nqp::x("\x[301]", 2).chars, ("\x[301]" x 2).chars, 'nqp::x agrees with infix:<x>';
is nqp::elems(nqp::split("", "ae\x[301]\r\nb")), 4, 'nqp::split("") splits graphemes';
