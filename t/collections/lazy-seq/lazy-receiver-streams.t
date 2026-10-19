use Test;

# A streaming method on a receiver that may not end answers a lazy stage over
# it (ADR-13891 §4, `method_table::lazy_stream`): an unbounded `Range` and a
# lazy `LazyList` reach the same streams, through a literal method name and a
# dynamic one alike. A finite lazy-looking receiver (a sequence with an end)
# is not streamed, and a `Range` keeps its own counting methods: the lazy
# refusals of a `LazyList` do not reach it.

plan 30;

is (1..*).skip(2).head(3), (3, 4, 5), 'Range.skip streams';
is (1..*).rotor(2).head(2), ((1, 2), (3, 4)), 'Range.rotor streams';
is (1..*).batch(2).head(2), ((1, 2), (3, 4)), 'Range.batch streams';
is (1..*).unique.head(3), (1, 2, 3), 'Range.unique streams';
is (1..*).squish.head(2), (1, 2), 'Range.squish streams';
ok (1..*).repeated.is-lazy, 'Range.repeated is lazy';
is (1..*).produce(&[+]).head(4), (1, 3, 6, 10), 'Range.produce streams';
is (1..*).pairs.head(2), (0 => 1, 1 => 2), 'Range.pairs streams';
is (1..*).kv.head(4), (0, 1, 1, 2), 'Range.kv streams';
is (1..*).keys.head(2), (0, 1), 'Range.keys streams';
is (1..*).antipairs.head(2), (1 => 0, 2 => 1), 'Range.antipairs streams';
is (^Inf).skip.head(2), (1, 2), 'skip with no count on ^Inf';
is ("a"..*).pairs.head(2), (0 => "a", 1 => "b"), 'a Str range streams';
is (1.5..*).batch(2).head(2), ((1.5, 2.5), (3.5, 4.5)), 'a Rat range streams';

my $m = 'skip';
is (1..*)."$m"(3).head(2), (4, 5), 'a dynamic name reaches the stream';
is (1...*)."$m"(3).head(2), (4, 5), 'a dynamic name on an infinite Seq';

is (1...*).map(* * 2).batch(2).head(2), ((2, 4), (6, 8)), 'a lazy map streams';
is (gather { take $_ for 1..* }).lazy.rotor(3).head(1), ((1, 2, 3),), 'a lazy gather streams';
is (1...*).pairs.head(1), (0 => 1,), 'an infinite Seq streams its pairs';
ok (1...*).flat.is-lazy, 'flat of an infinite Seq stays lazy';

nok (1, 2 ... 10).pairs.is-lazy, 'a finite sequence is not streamed';
nok (1, 2 ... 10).skip.is-lazy, 'a finite sequence skips eagerly';
is (1, 2 ... 10).skip(8), (9, 10), 'and answers the same elements';

is (1..*).sum, Inf, 'Range.sum answers itself';
is (1..*).min, 1, 'Range.min answers itself';
is (1..^*).sum, Inf, 'an exclusive unbounded Range sums to Inf';
is (^Inf).sum, Inf, '^Inf sums to Inf';
is (1.5..*).sum, Inf, 'a Rat unbounded Range sums to Inf';
is (1..9223372036854775806).sum, 42535295865117307919086767873688862721, 'a sum past i64 is exact';
is (1..*).flat.head(2), (1, 2), 'Range.flat is Range own method';
