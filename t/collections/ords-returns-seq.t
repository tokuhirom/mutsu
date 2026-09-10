use Test;

# `.ords` / `ords()` return a Seq (like `.comb`/`comb()`), not a List, and the
# Seq round-trips through `.chrs` (which must accept a Seq argument, not just a
# List/Range).

plan 12;

is "abc".ords.WHAT.^name, 'Seq', '"abc".ords is a Seq';
is ords("abc").WHAT.^name, 'Seq', 'ords("abc") is a Seq';

is-deeply "abc".ords.List, (97, 98, 99), '.ords values';
is-deeply ords("abc").List, (97, 98, 99), 'ords() values';

is "abc".ords.raku, '(97, 98, 99).Seq', '.ords renders as a Seq';
is "".ords.raku, '().Seq', 'empty .ords renders as a Seq';

# Round-trips
is "abc".ords.chrs, "abc", '.ords.chrs round-trips (method chrs accepts a Seq)';
is chrs(ords("ABCDEFGHIJK")), "ABCDEFGHIJK", 'chrs(ords()) round-trips';

# Common downstream usage keeps working
is "abc".ords[1], 98, '.ords is indexable';
is "abc".ords.sum, 294, '.ords.sum';
is "abc".ords.elems, 3, '.ords.elems';
is-deeply "abc".ords.map(*+1).List, (98, 99, 100), '.ords.map';
