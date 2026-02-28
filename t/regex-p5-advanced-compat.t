use Test;

plan 8;

is(("aaaaaaaaaa" ~~ rx:P5/^(a\1?){4}$/ && $0), "aaaa", 'P5 backref in quantified capture');
nok("aB" ~~ rx:P5/((?i)a)b/, 'inline (?i) in capture does not leak outside capture');
is(("aB" ~~ rx:P5/(?i)((?-i)a)b/ && $0), "a", 'group-local (?-i) only affects the inner group');
is(("cabd" ~~ rx:P5/a(?{})b/ && $/), "ab", 'P5 (?{}) is treated as zero-width no-op');
is(("cabd" ~~ rx:P5/a(?{"\{"})b/ && $/), "ab", 'P5 (?{"..."}) is treated as zero-width no-op');
is(("ab4Ab" ~~ rx:P5/(?i)(ab)\d\1/ && $0), "ab", '(?i) backreference is case-insensitive');
is(("a\nb\nc\n" ~~ rx:P5/((?s).)c(?!.)/ && $0), "\n", '(?s) dot matches newline');
is(("a\nb\nc\n" ~~ rx:P5/((?s)b.)c(?!.)/ && $0), "b\n", '(?s) works with grouped captures');
