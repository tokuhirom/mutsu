use Test;

# ADR-11276 slice 3C: the collections' small coercions are handler rows:
# `Slip` (List, Array), `List` (Array, Map), `list` (Map and the six quant
# hashes), `hash` (Map), `default` (Array, Hash) and `Pair.Pair`. Every
# answer below was checked against Rakudo.

plan 7;

subtest 'Slip', {
    plan 5;
    my @array = 1, 2, 3;
    my @holey = 1, 2, 3;
    @holey[1]:delete;
    is @array.Slip.raku, 'slip(1, 2, 3)', 'Array.Slip';
    is (1, 2, 3).Slip.raku, 'slip(1, 2, 3)', 'List.Slip';
    is (1, Nil, 3).Slip.raku, 'slip(1, Nil, 3)', 'a List keeps its Nil';
    my @defaulted is default(7) = 1, 2;
    is @defaulted.Slip.raku, 'slip(1, 2)', 'an Array with a default';
    ok @holey.Slip.elems == 3, 'a hole is still an element';
}

subtest 'List', {
    plan 3;
    my @array = 1, 2, 3;
    is @array.List.raku, '(1, 2, 3)', 'Array.List';
    is @array.List.^name, 'List', 'a List';
    my %hash = a => 1, b => 2;
    is %hash.List.sort.raku, '(:a(1), :b(2)).Seq', 'Hash.List is its pairs';
}

subtest 'list of a Map', {
    plan 4;
    my %hash = a => 1, b => 2;
    my $map = Map.new((a => 1, b => 2));
    is %hash.list.sort.raku, '(:a(1), :b(2)).Seq', 'Hash.list';
    is $map.list.sort.raku, '(:a(1), :b(2)).Seq', 'Map.list';
    is %hash.list.^name, 'List', 'a List';
    is $map.List.^name, 'List', 'Map.List';
}

subtest 'list of a quant hash', {
    plan 5;
    is set(<a b>).list.sort.raku, '(:a, :b).Seq', 'Set';
    is bag(<a a b>).list.sort.raku, '(:a(2), :b(1)).Seq', 'Bag';
    is (a => 1.5).Mix.list.raku, '(:a(1.5),)', 'Mix';
    is BagHash.new(<q q>).list.raku, '(:q(2),)', 'BagHash';
    is SetHash.new(<x>).list.elems, 1, 'SetHash';
}

subtest 'hash of a Hash', {
    plan 2;
    my %hash = a => 1;
    ok %hash.hash === %hash, 'Hash.hash is the hash itself';
    my $map = Map.new((a => 1));
    is $map.hash.^name, 'Map', 'Map.hash is a Map';
}

subtest 'default', {
    plan 6;
    my @array = 1, 2;
    my @defaulted is default(7) = 1, 2;
    my %hash = a => 1;
    my %defaulted is default(5);
    is @array.default.raku, 'Any', 'Array.default';
    is @defaulted.default, 7, 'an Array with a default';
    is %hash.default.raku, 'Any', 'Hash.default';
    is %defaulted.default, 5, 'a Hash with a default';
    is @defaulted[5], 7, 'the default is what a missing element reads as';
    is %defaulted<missing>, 5, 'and a missing key';
}

subtest 'Pair.Pair', {
    plan 2;
    is (a => 1).Pair.raku, ':a(1)', 'a string-keyed Pair';
    is (1 => 2).Pair.raku, '1 => 2', 'a Pair with a data key';
}
