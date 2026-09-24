use v6;
use Test;

# Two regex/rakudo divergences found while probing the character classes
# (#9233). Expected values were measured with rakudo.

plan 11;

# \h, \H and <[\h]> share one horizontal-space set, which includes U+180E
# MONGOLIAN VOWEL SEPARATOR (a Cf that MoarVM still lists as \h).
ok  "\x[180E]" ~~ /^\h$/,    '\h matches U+180E';
nok "\x[180E]" ~~ /\H/,      '\H does not';
ok  "\x[180E]" ~~ /<[\h]>/,  '<[\h]> matches U+180E';
nok "\x[180E]" ~~ /<-[\h]>/, '<-[\h]> does not';
ok  "\x[1680]\t\x[3000]" ~~ /^\h+$/, 'the other horizontal spaces still match';

# A spacing mark (Mc) extends a grapheme only when UAX #29 calls it a
# SpacingMark. U+102B is one of the ~30 excluded, so "x\x[102B]" is two
# graphemes, and a regex must see the same boundary .chars does.
is "x\x[102B]".chars, 2, 'x + U+102B is two graphemes';
is ("x\x[102B]" ~~ /x/).to, 1, '/x/ matches the x alone';
ok "x\x[102B]" ~~ /x>>/, '>> after x holds before an excluded Mc';
ok "a\x[102B]" ~~ /a<?wb>/, '<?wb> too';

# A real SpacingMark (U+0903) still joins its base.
is "x\x[903]".chars, 1, 'x + U+0903 is one grapheme';
nok "x\x[903]" ~~ /x/, 'so /x/ does not match inside it';
