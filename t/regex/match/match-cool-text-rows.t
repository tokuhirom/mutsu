use Test;

plan 32;

# Rakudo's `Match` is `Capture is Cool`, so the text methods neither `Match`
# nor `Capture` declares are `Cool`'s, which read the matched text
# (`self.Str.METHOD`). In mutsu a `Match` reaches `Cool`'s search, split,
# substitution and formatting rows (ADR-11276 §9.89); a grammar cursor, which
# has no shape, reaches the same rows through `Match`. Every expectation below
# was checked against Rakudo.
my $m = "ab12,3cd" ~~ /\d+ ',' \d/;

is-deeply $m.contains("2"), True, 'contains';
is-deeply $m.contains(4), False, 'contains with a numeric needle';
is-deeply $m.starts-with("12"), True, 'starts-with';
is-deeply $m.ends-with(",3"), True, 'ends-with';
is-deeply $m.index(","), 2, 'index';
is-deeply $m.rindex("3"), 3, 'rindex';
is-deeply $m.index("x"), Nil, 'index that misses';
is $m.substr(1), "2,3", 'substr with a start';
is $m.substr(1, 2), "2,", 'substr with a start and a length';
is-deeply $m.split(","), ("12", "3").Seq, 'split on a string';
is-deeply $m.split(/<[,]>/), ("12", "3").Seq, 'split on a regex';
is $m.fmt("<%s>"), "<12,3>", 'fmt';
is-deeply $m.comb(2), ("12", ",3").Seq, 'comb with a chunk size';
is-deeply $m.comb("1"), ("1",).Seq, 'comb with a needle';
is $m.samecase("Ab"), "12,3", 'samecase';
is $m.indent(2), "  12,3", 'indent';
is $m.subst("1", "x"), "x2,3", 'subst';
is $m.trans("2" => "z"), "1z,3", 'trans';
is-deeply $m.indices("2"), (1,), 'indices';
is $m.uc, "12,3", 'a text method with no argument';
throws-like { $m.contains(Str) }, Exception, 'a type-object needle binds no candidate';

grammar G { token TOP { <w>+ }; token w { \w } }
my $cursor = G.parse("ab1");
is-deeply $cursor.contains("b"), True, 'a grammar cursor reaches contains';
is-deeply $cursor.index("1"), 2, 'a grammar cursor reaches index';
is $cursor.substr(1), "b1", 'a grammar cursor reaches substr';
is-deeply $cursor.split("b"), ("a", "1").Seq, 'a grammar cursor reaches split';
is $cursor<w>[1].fmt("[%s]"), "[b]", 'a sub-cursor reaches fmt';

# The arms these rows replaced also answered other receivers; the
# interpreter answers them the same way.
is-deeply "abc".contains("b"), True, 'Str.contains';
throws-like { "abc".contains(Str) }, Exception, 'Str.contains with a type-object needle';
is-deeply (1..13).index("3"), 4, 'Range.index reads its string form';
is-deeply 123456.comb(4), ("1234", "56").Seq, 'Int.comb with a chunk size';
is "%05d".sprintf(42), "00042", 'Str.sprintf with one argument';
is-deeply "a1b22".comb(/\d+/), ("1", "22").Seq, 'comb with a regex takes the interpreter';
