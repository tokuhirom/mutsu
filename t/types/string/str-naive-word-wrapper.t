use Test;

# `Str.naive-word-wrapper`: rakudo's implementation-detail wrapper, which
# vendored core modules (upstream NativeCall) call (ADR-11203, #11208).

plan 9;

is "abc def ghi".naive-word-wrapper, "abc def ghi", 'short text is one line at the default width';
is "aaa bbb ccc".naive-word-wrapper(:max(8)), "aaa bbb\nccc", ':max wraps between words';
is "aaa bbb ccc".naive-word-wrapper(:max(10), :indent("  ")), "  aaa bbb\n  ccc",
    ':indent prefixes every line and counts towards the width';
is "abcdefghij k".naive-word-wrapper(:max(5)), "abcdefghij\nk",
    'a word wider than :max on an empty line becomes its own line';
is "\e[31mred\e[0m x".naive-word-wrapper(:max(6)), "\e[31mred\e[0m x",
    'colour escape codes do not count towards the width';
is "  spaced\tout\n words ".naive-word-wrapper, "spaced out words", 'any whitespace separates words';
is "".naive-word-wrapper, "", 'the empty string wraps to the empty string';
is "a b".naive-word-wrapper(:unknown), "a b", 'an unknown named argument is ignored';
my $s = "p q";
is $s.naive-word-wrapper(:max(2)), "p\nq", 'works on a Str held in a container';
