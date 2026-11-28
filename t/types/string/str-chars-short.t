use Test;

# `.chars` counts a short string in place, without building a grapheme index;
# the count must match the indexed one for every shape of string.

plan 9;

is "Woof!".chars, 5, 'ASCII';
is "".chars, 0, 'empty';
is "a\r\nb".chars, 3, 'CRLF is one grapheme';
is "e\x[301]x".chars, 2, 'combining mark joins its base';
is "日本語".chars, 3, 'multi-byte codepoints';
is "🇯🇵🇺🇸".chars, 2, 'regional-indicator pairs';
my $long = "ab" x 300;
is $long.chars, 600, 'a string past the index-cache threshold';
is ("x\x[301]" x 200).chars, 200, 'a long non-ASCII string';
my $c = 0;
$c += "Rex has 4 legs".chars for ^100;
is $c, 1400, 'repeated counts in a loop';
