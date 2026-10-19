use Test;

# Rows for methods Rakudo declares on a class besides the one that serves the
# receiver (ADR-11276): each owner's method table carries the name, and the
# answer is the shared handler's.
plan 42;

for Array, Bag, BagHash, List, Map, Mix, MixHash, Pair, Range, Seq, Set, SetHash -> $type {
    ok $type.^can('iterator'), "{$type.^name}.^can('iterator')";
}

is-deeply Set.of, Bool, 'Set.of';
is-deeply SetHash.new(1).of, Bool, 'SetHash.of';
is-deeply Bag.of, UInt, 'Bag.of';
is-deeply BagHash.new(1).of, UInt, 'BagHash.of';
is-deeply Mix.of, Real, 'Mix.of';
is-deeply MixHash.new(1).of, Real, 'MixHash.of';

is-deeply (a => [1, 2]).freeze, [1, 2], 'Pair.freeze is the value';
is-deeply (3 => 4).freeze, 4, 'Pair.freeze of an Int value';

is-deeply (3, 1, 2).Seq.sort.List, (1, 2, 3), 'Seq.sort';
is-deeply {b => 1, a => 2}.Map.sort.List, (a => 2, b => 1).List.sort({ $^a.key cmp $^b.key }), 'Map.sort';
is-deeply set(<a b>).minpairs.sort.List, (a => True, b => True), 'Set.minpairs';
is-deeply set(<a b>).maxpairs.sort.List, (a => True, b => True), 'Set.maxpairs';
is-deeply Map.new((a => 1)).AT-KEY('a'), 1, 'Map.AT-KEY';
is Map.new((a => 1)).Str, "a\t1", 'Map.Str';
is-deeply Map.new((a => 1)).clone, Map.new((a => 1)), 'Map.clone';
is-deeply (1, 2, 3).Seq.eager, (1, 2, 3).Seq.eager, 'Seq.eager';
is-deeply (1..5).first(* > 2), 3, 'Range.first';
is-deeply 65.chrs, 'A', 'Cool.chrs';

for Int, Num, Rat, Complex, Date, DateTime, Duration, Instant, Pair, Seq, Set -> $type {
    ok $type.^can('new'), "{$type.^name}.^can('new')";
}
is-deeply Pair.new(1, 2), 1 => 2, 'Pair.new';
