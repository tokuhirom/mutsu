use Test;

# `\c[0x0041]` names a codepoint by a radix-prefixed number, in a string, a
# regex atom and a character class (Lang::Transliterate's Syriac tables).
plan 7;

is "\c[0x0730]".ords, (1840,), 'in a string';
is ("xxAyy" ~~ /\c[0x0041]/).Str, 'A', 'as a regex atom';
is ("xxAyy" ~~ /<[\c[0x0041]]>/).Str, 'A', 'in a character class';
is ("xxBCyy" ~~ /<[\c[0x0042]..\c[0x0043]]>+/).Str, 'BC', 'as range endpoints';
is ("xxAyy" ~~ /\c[0o101]/).Str, 'A', 'octal prefix';
is ("xxAyy" ~~ /\c[0b1000001]/).Str, 'A', 'binary prefix';
nok "x0x0041y" ~~ /\c[0x0041]/, 'the digits themselves are not matched';
