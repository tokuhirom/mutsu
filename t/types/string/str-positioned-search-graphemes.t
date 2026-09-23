use Test;

plan 20;

# Positioned string searches (issue #9146).
#
# 1. `.contains(Regex, $pos)` matches against the WHOLE subject starting at
#    `$pos` (`:c($pos)` semantics), not against a copied suffix: a lookbehind
#    or `<<` sees what precedes `$pos`, and `^` stays anchored at the start of
#    the string.
# 2. `$pos` (and the needle's length) count graphemes, like `.substr` and
#    `.index`, in `.substr-eq`, `.contains($str, $pos)` and `substr-rw`.
#    `"q\x[301]"` has no precomposed form, so it stays two codepoints but is
#    one grapheme.

# --- 1. Regex needle sees the text before $pos ---
ok  'abc'.contains(/<?after b>c/, 2), 'lookbehind sees the char before $pos';
nok 'abc'.contains(/<?after x>c/, 2), 'a failing lookbehind still fails';
ok  'a c'.contains(/<<c/, 2), '<< at a real word start after $pos';
nok 'abc'.contains(/<<c/, 2), 'no word start at $pos inside a word';
nok 'abc'.contains(/^c/, 2), '^ anchors to the start of the string, not $pos';
ok  'abc'.contains(/b/, 1), 'a plain regex still matches from $pos';
nok 'abc'.contains(/a/, 1), 'and does not look before $pos for its match';
nok 'abc'.contains(/c/, 3), 'nothing to find at the end';

# --- 2. grapheme positions ---
my $s = "q\x[301]ab";
is $s.chars, 3, 'q + combining acute is one grapheme';
ok  $s.substr-eq('a', 1), 'substr-eq: $pos counts graphemes';
ok  $s.substr-eq('ab', 1), 'substr-eq: multi-char needle at a grapheme $pos';
nok $s.substr-eq('a', 2), 'substr-eq: grapheme 2 is b';
ok  $s.substr-eq('AB', 1, :i), 'substr-eq :i counts graphemes';
ok  $s.substr-eq('ab', 1, :m), 'substr-eq :m counts graphemes';
ok  $s.contains('a', 1), 'contains($str, $pos) at a grapheme $pos';
nok $s.contains('a', 2), 'contains($str, $pos) skips whole graphemes';
nok $s.contains('A', 2, :i), 'contains($str, $pos, :i) skips whole graphemes';
ok  $s.contains(/<?after "q\x[301]">a/, 1), 'regex: grapheme $pos plus lookbehind';

# (Distinct strings per variable: the sub form of substr-rw can pick the wrong
# variable when two hold an equal string, which is #9183.)
{
    my $t = "q\x[301]cd";
    substr-rw($t, 1, 1) = 'X';
    is $t, "q\x[301]Xd", 'substr-rw replaces the grapheme at $pos';
}
{
    my $t = "q\x[301]ef";
    $t.substr-rw(0, 1) = 'X';
    is $t, 'Xef', 'substr-rw replaces a whole multi-codepoint grapheme';
}
