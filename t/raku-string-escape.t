use Test;

# Str.raku escapes control characters like Rakudo: named escapes for
# \0 \b \t \n \r, and \x[HEX] (upper-case, no leading zero) for every other
# Unicode Cc control char. This must apply both to a bare string and to a
# string nested inside a list/Seq (which used to leave control chars raw).
plan 16;

# Named control escapes.
is 0.chr.raku,  '"\0"',  'NUL escapes to \0';
is 8.chr.raku,  '"\b"',  'BS escapes to \b';
is 9.chr.raku,  '"\t"',  'TAB escapes to \t';
is 10.chr.raku, '"\n"',  'LF escapes to \n';
is 13.chr.raku, '"\r"',  'CR escapes to \r';

# Other control chars use \x[HEX], upper-case, no leading zero.
is 27.chr.raku,   '"\x[1B]"', 'ESC escapes to \x[1B]';
is 12.chr.raku,   '"\x[C]"',  'FF escapes to \x[C]';
is 1.chr.raku,    '"\x[1]"',  'SOH escapes to \x[1]';
is 127.chr.raku,  '"\x[7F]"', 'DEL escapes to \x[7F]';
is 0x85.chr.raku, '"\x[85]"', 'NEL (C1) escapes to \x[85]';

# Nested in a Seq/List: the tab must be escaped, not left raw.
is "a\tb".comb.raku, '("a", "\t", "b").Seq', 'tab inside a Seq is escaped';
is ("x\ny",).raku,   '("x\ny",)',            'newline inside a list is escaped';

# Sigil / interpolation metacharacters are still escaped.
is 'a@b'.raku, '"a\@b"', '@ is escaped';
is 'a%b'.raku, '"a\%b"', '% is escaped';
is 'a&b'.raku, '"a\&b"', '& is escaped';

# Non-control high characters pass through unchanged.
is "café".raku, '"café"', 'non-control chars pass through';
