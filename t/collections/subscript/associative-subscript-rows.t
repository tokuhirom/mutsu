use Test;

# ADR-11276 slice 3C: AT-KEY, EXISTS-KEY and ACCEPTS of the associative
# collections are handler rows (Hash, Map, the six quant hashes, Pair, Capture
# and Range own them). Every answer below was checked against Rakudo.

plan 6;

subtest 'Hash', {
    plan 4;
    my %h = a => 1, b => 2;
    is %h.AT-KEY('a'), 1, 'AT-KEY';
    is %h.AT-KEY('z').raku, 'Any', 'a missing key is the default';
    ok %h.EXISTS-KEY('b'), 'EXISTS-KEY';
    nok %h.EXISTS-KEY('q'), 'EXISTS-KEY of a missing key';
}

subtest 'quant hashes', {
    plan 12;
    my $set = set <a b c>;
    my $sethash = SetHash.new(<x y>);
    my $bag = bag <a a b>;
    my $baghash = BagHash.new(<q q q r>);
    my $mix = (a => 1.5, b => 2).Mix;
    my $mixhash = (a => 0.5, c => 1).MixHash;
    is $set.AT-KEY('a'), True, 'Set.AT-KEY';
    is $sethash.AT-KEY('a'), False, 'SetHash.AT-KEY of a missing element';
    is $bag.AT-KEY('a'), 2, 'Bag.AT-KEY';
    is $bag.AT-KEY('zz'), 0, 'Bag.AT-KEY of a missing element';
    is $baghash.AT-KEY('q'), 3, 'BagHash.AT-KEY';
    is $mix.AT-KEY('a'), 1.5, 'Mix.AT-KEY';
    is $mixhash.AT-KEY('c'), 1, 'MixHash.AT-KEY';
    ok $set.EXISTS-KEY('a'), 'Set.EXISTS-KEY';
    nok $set.EXISTS-KEY('zz'), 'Set.EXISTS-KEY of a missing element';
    ok $bag.EXISTS-KEY('b'), 'Bag.EXISTS-KEY';
    ok $mix.EXISTS-KEY('b'), 'Mix.EXISTS-KEY';
    nok $mixhash.EXISTS-KEY('b'), 'MixHash.EXISTS-KEY of a missing element';
}

subtest 'Pair', {
    plan 8;
    my $pair = a => 5;
    is $pair.AT-KEY('a'), 5, 'AT-KEY';
    is $pair.AT-KEY('b').raku, 'Nil', 'AT-KEY of another key is Nil';
    ok $pair.EXISTS-KEY('a'), 'EXISTS-KEY';
    nok $pair.EXISTS-KEY('b'), 'EXISTS-KEY of another key';
    my $data = 1 => 'x';
    is $data.AT-KEY(1), 'x', 'a Pair with a data key: AT-KEY';
    is $data.AT-KEY(2).raku, 'Nil', 'AT-KEY of another key';
    ok $data.EXISTS-KEY(1), 'EXISTS-KEY';
    nok $data.EXISTS-KEY(2), 'EXISTS-KEY of another key';
}

subtest 'Capture', {
    plan 4;
    my $capture = \(1, 2, a => 3);
    is $capture.AT-KEY('a'), 3, 'AT-KEY reads a named argument';
    is $capture.AT-KEY('z').raku, 'Nil', 'AT-KEY of a missing name is Nil';
    ok $capture.EXISTS-KEY('a'), 'EXISTS-KEY';
    nok $capture.EXISTS-KEY('z'), 'EXISTS-KEY of a missing name';
}

subtest 'ACCEPTS on a quant hash', {
    plan 10;
    my $set = set <a b c>;
    my $bag = bag <a a b>;
    my $mix = (a => 1.5, b => 2).Mix;
    ok $set.ACCEPTS(set <a b c>), 'an equal Set';
    nok $set.ACCEPTS(set <a b>), 'a smaller Set';
    nok $set.ACCEPTS(5), 'a non-Set';
    ok $bag.ACCEPTS(bag <a a b>), 'an equal Bag';
    nok $bag.ACCEPTS(bag <a b>), 'a Bag with other counts';
    nok $bag.ACCEPTS(set <a b>), 'a Set is not a Bag';
    ok $mix.ACCEPTS((a => 1.5, b => 2).Mix), 'an equal Mix';
    nok $mix.ACCEPTS((a => 1, b => 2).Mix), 'a Mix with other weights';
    ok SetHash.new(<x y>).ACCEPTS(SetHash.new(<x y>)), 'SetHash';
    nok BagHash.new(<q q q r>).ACCEPTS(set <q>), 'BagHash against a Set';
}

subtest 'ACCEPTS on a Pair and a Range', {
    plan 14;
    my $bag = bag <a a b>;
    ok (a => 5).ACCEPTS(%(a => 5)), 'a Pair matches an equal Hash entry';
    nok (a => 5).ACCEPTS(%(a => 6)), 'a Pair against another value';
    ok (a => 1).ACCEPTS(bag <a>), 'a Pair against a Bag count';
    nok (a => 2).ACCEPTS(bag <a>), 'a Pair against another Bag count';
    ok (a => True).ACCEPTS(set <a>), 'a Pair against a Set member';
    ok (a => 1.5).ACCEPTS((a => 1.5).Mix), 'a Pair against a Mix weight';
    ok (a => 5).ACCEPTS((a => 5)), 'a Pair against an equal Pair';
    nok (a => 5).ACCEPTS((b => 5)), 'a Pair against another Pair';
    ok (1..5).ACCEPTS(3), 'a Range contains a value';
    nok (1..5).ACCEPTS(7), 'a Range does not contain 7';
    ok (1..5).ACCEPTS(2..3), 'a Range contains a subrange';
    nok (1..5).ACCEPTS(0..3), 'a Range does not contain 0..3';
    ok ('a'..'c').ACCEPTS('b'), 'a string Range';
    nok (1^..5).ACCEPTS(1), 'an excluded start';
}
