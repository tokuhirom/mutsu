use Test;
plan 28;

# `samecase`, `samemark`, `indent`, `encode`, `parse-base`, `substr-eq`,
# `naive-word-wrapper`, `Date` and `DateTime` are method-table rows on `Str`
# and `Cool` (ADR-11276 §9.74). Expectations checked against raku.
is "hello".samecase("AbCdE"), "HeLlO", 'Str.samecase';
is 12345.samecase("Ab"), "12345", 'Cool.samecase stringifies';
is "abc".samemark("ä"), "äb̈c̈", 'Str.samemark';
is 123.samemark("é"), "1́2́3́", 'Cool.samemark';
is "a\nb".indent(2), "  a\n  b", 'Str.indent';
is 42.indent(2), "  42", 'Cool.indent';
is "  a\n  b".indent(*), "a\nb", 'Str.indent(*) removes the common indent';
is "a\tb\n\tc".indent(-1), "a\tb\n       c", 'a tab is expanded before narrowing';

is "aé".encode, utf8.new(97, 195, 169), 'Str.encode';
is 42.encode, utf8.new(52, 50), 'Cool.encode';
isa-ok "aé".NFD.encode, Buf, 'Uni.encode answers a Buf';
is "aé".NFD.encode.list, (97, 101, 204, 129), 'Uni.encode keeps the codepoints';

is "ff".parse-base(16), 255, 'Str.parse-base';
is "-11".parse-base(2), -3, 'a sign';
is "1.1".parse-base(2), 1.5, 'a fraction';
throws-like { "11".parse-base("2") }, X::Multi::NoMatch, 'a Str radix binds no candidate';
throws-like { "11".parse-base(2.0) }, X::Multi::NoMatch, 'nor a Rat radix';

ok "abcd".substr-eq("bc", 1), 'Str.substr-eq';
nok "abcd".substr-eq("bc", 2), 'Str.substr-eq elsewhere';
nok "abcd".substr-eq("bc", 10), 'a position past the end is False';
ok "abcd".substr-eq("", 4), 'the empty needle at the end';
ok "abcd".substr-eq("ab"), 'the position defaults to 0';
isa-ok "abcd".substr-eq("bc", -1), Failure, 'a negative position fails';

is "aaa bbb ccc ddd".naive-word-wrapper(:max(8)), "aaa bbb\nccc ddd", 'Str.naive-word-wrapper';
is "2020-01-02".Date, Date.new(2020, 1, 2), 'Str.Date';
is "2020-01-02T03:04:05Z".DateTime, DateTime.new(2020, 1, 2, 3, 4, 5), 'Str.DateTime';
throws-like { "bad".Date }, X::Temporal::InvalidFormat, 'Str.Date of a bad string';
is "ab".Stringy, "ab", 'Str.Stringy is the string';
