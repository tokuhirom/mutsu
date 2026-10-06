use Test;

# ADR-11276 slice 3C: the quant hashes' own views (keys, values, kv, pairs,
# antipairs, kxxv, invert, total, Numeric, elems, default, of, hash) are
# handler rows owned by Set, SetHash, Bag, BagHash, Mix and MixHash. Every
# answer below was checked against Rakudo.

plan 9;

my $set = set <a b c>;
my $sethash = SetHash.new(<a b>);
my $bag = bag <a a b c c c>;
my $baghash = BagHash.new(<x x y>);
my $mix = (a => 1.5, b => 2.5).Mix;
my $mixhash = (a => 0.5, b => 1.5).MixHash;

sub pair-strs(@pairs) { @pairs.map({ .key.raku ~ '=>' ~ .value.raku }).sort.join(' ') }

subtest 'keys', {
    plan 6;
    is $set.keys.sort.join(' '), 'a b c', 'Set';
    is $sethash.keys.sort.join(' '), 'a b', 'SetHash';
    is $bag.keys.sort.join(' '), 'a b c', 'Bag';
    is $baghash.keys.sort.join(' '), 'x y', 'BagHash';
    is $mix.keys.sort.join(' '), 'a b', 'Mix';
    is $mixhash.keys.sort.join(' '), 'a b', 'MixHash';
}

subtest 'values', {
    plan 6;
    is $set.values.map(*.raku).join(' '), 'Bool::True Bool::True Bool::True', 'Set';
    is $sethash.values.map(*.raku).join(' '), 'Bool::True Bool::True', 'SetHash';
    is $bag.values.sort.join(' '), '1 2 3', 'Bag';
    is $baghash.values.sort.join(' '), '1 2', 'BagHash';
    is $mix.values.sort.join(' '), '1.5 2.5', 'Mix';
    is $mixhash.values.sort.join(' '), '0.5 1.5', 'MixHash';
}

subtest 'kv, pairs and antipairs', {
    plan 12;
    is $set.kv.elems, 6, 'Set.kv';
    is $bag.kv.elems, 6, 'Bag.kv';
    is $mix.kv.elems, 4, 'Mix.kv';
    is $mixhash.kv.elems, 4, 'MixHash.kv';
    is pair-strs($bag.pairs), '"a"=>2 "b"=>1 "c"=>3', 'Bag.pairs';
    is pair-strs($baghash.pairs), '"x"=>2 "y"=>1', 'BagHash.pairs';
    is pair-strs($mix.pairs), '"a"=>1.5 "b"=>2.5', 'Mix.pairs';
    is pair-strs($set.pairs), '"a"=>Bool::True "b"=>Bool::True "c"=>Bool::True', 'Set.pairs';
    is pair-strs($bag.antipairs), '1=>"b" 2=>"a" 3=>"c"', 'Bag.antipairs';
    is pair-strs($mix.antipairs), '1.5=>"a" 2.5=>"b"', 'Mix.antipairs';
    is pair-strs($set.antipairs), 'Bool::True=>"a" Bool::True=>"b" Bool::True=>"c"', 'Set.antipairs';
    is $bag.pairs.^name, 'Seq', 'a Seq';
}

subtest 'object keys survive', {
    plan 6;
    my $objects = set 1, 2.5, 'x';
    is $objects.keys.map(*.raku).sort.join(' '), '"x" 1 2.5', 'Set.keys keeps the types';
    is pair-strs($objects.pairs), '"x"=>Bool::True 1=>Bool::True 2.5=>Bool::True',
        'Set.pairs keeps the types';
    is pair-strs($objects.antipairs), 'Bool::True=>"x" Bool::True=>1 Bool::True=>2.5',
        'Set.antipairs keeps the types';
    my $b = bag 1, 1, 2;
    is pair-strs($b.pairs), '1=>2 2=>1', 'Bag of Ints';
    is $b.kxxv.sort.join(' '), '1 1 2', 'Bag.kxxv of Ints';
    is $b.invert.map(*.raku).sort.join(' '), '1 => 2 2 => 1', 'Bag.invert';
}

subtest 'kxxv, invert and Numeric', {
    plan 8;
    is $bag.kxxv.sort.join(' '), 'a a b c c c', 'Bag.kxxv';
    is $baghash.kxxv.sort.join(' '), 'x x y', 'BagHash.kxxv';
    is $bag.invert.map(*.raku).sort.join(' '), '1 => "b" 2 => "a" 3 => "c"', 'Bag.invert';
    is $mix.invert.map(*.raku).sort.join(' '), '1.5 => "a" 2.5 => "b"', 'Mix.invert';
    is $bag.Numeric, 6, 'Bag.Numeric is the total';
    is $baghash.Numeric, 3, 'BagHash.Numeric';
    is $mix.Numeric, 4, 'Mix.Numeric';
    is $mixhash.Numeric, 2, 'MixHash.Numeric';
}

subtest 'Mix kxxv is unsupported', {
    plan 2;
    throws-like { $mix.kxxv }, X::AdHoc, :message('.kxxv is not supported on a Mix'),
        'Mix.kxxv is not supported';
    throws-like { $mixhash.kxxv }, X::AdHoc, :message('.kxxv is not supported on a MixHash'),
        'MixHash.kxxv is not supported';
}

subtest 'total and elems', {
    plan 12;
    is $set.total, 3, 'Set.total';
    is $sethash.total, 2, 'SetHash.total';
    is $bag.total, 6, 'Bag.total';
    is $baghash.total, 3, 'BagHash.total';
    is $mix.total, 4, 'Mix.total';
    is $mixhash.total, 2, 'MixHash.total';
    is $set.elems, 3, 'Set.elems';
    is $sethash.elems, 2, 'SetHash.elems';
    is $bag.elems, 3, 'Bag.elems counts the distinct elements';
    is $baghash.elems, 2, 'BagHash.elems';
    is $mix.elems, 2, 'Mix.elems';
    is $mixhash.elems, 2, 'MixHash.elems';
}

subtest 'default and of', {
    plan 12;
    is $set.default.raku, 'Bool::False', 'Set.default';
    is $sethash.default.raku, 'Bool::False', 'SetHash.default';
    is $bag.default, 0, 'Bag.default';
    is $baghash.default, 0, 'BagHash.default';
    is $mix.default, 0, 'Mix.default';
    is $mixhash.default, 0, 'MixHash.default';
    is $set.of.raku, 'Bool', 'Set.of';
    is $sethash.of.raku, 'Bool', 'SetHash.of';
    is $bag.of.raku, 'UInt', 'Bag.of';
    is $baghash.of.raku, 'UInt', 'BagHash.of';
    is $mix.of.raku, 'Real', 'Mix.of';
    is $mixhash.of.raku, 'Real', 'MixHash.of';
}

subtest 'hash and empty receivers', {
    plan 7;
    is pair-strs($set.hash.pairs), '"a"=>Bool::True "b"=>Bool::True "c"=>Bool::True', 'Set.hash';
    is pair-strs($bag.hash.pairs), '"a"=>2 "b"=>1 "c"=>3', 'Bag.hash';
    is pair-strs($mix.hash.pairs), '"a"=>1.5 "b"=>2.5', 'Mix.hash';
    ok $set.hash ~~ Hash, 'a Hash';
    is set().keys.elems, 0, 'an empty Set has no keys';
    is bag().total, 0, 'an empty Bag totals 0';
    is set().total, 0, 'an empty Set totals 0';
}
