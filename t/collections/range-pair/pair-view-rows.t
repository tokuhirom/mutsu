use Test;

# ADR-11276 slice 3C: Pair's own views (keys, values, kv, pairs, antipairs,
# invert) are handler rows owned by Pair, for both flavours of Pair (a
# string-keyed one and one with any key). Every answer below was checked
# against Rakudo.

plan 4;

subtest 'a string-keyed Pair', {
    plan 8;
    my $pair = a => 5;
    is $pair.keys.raku, '("a",).Seq', 'keys';
    is $pair.values.raku, '(5,).Seq', 'values';
    is $pair.kv.raku, '("a", 5).Seq', 'kv';
    is $pair.pairs.raku, '(:a(5),).Seq', 'pairs';
    is $pair.antipairs.raku, '(5 => "a",).Seq', 'antipairs swaps the pair';
    is $pair.invert.raku, '(5 => "a",).Seq', 'invert';
    is $pair.antipairs.elems, 1, 'one antipair';
    is $pair.pairs[0] === $pair, True, 'pairs is the pair itself';
}

subtest 'a Pair with a data key', {
    plan 6;
    my $pair = 1 => 'x';
    is $pair.keys.raku, '(1,).Seq', 'keys keeps the key object';
    is $pair.values.raku, '("x",).Seq', 'values';
    is $pair.kv.raku, '(1, "x").Seq', 'kv';
    is $pair.pairs.raku, '(1 => "x",).Seq', 'pairs';
    is $pair.antipairs.raku, '(:x(1),).Seq', 'antipairs';
    is $pair.invert.raku, '(:x(1),).Seq', 'invert';
}

subtest 'a Pair with a list key and an array value', {
    plan 4;
    my $pair = <k v> => [1, 2];
    is $pair.keys.raku, '(("k", "v"),).Seq', 'keys';
    is $pair.values.raku, '([1, 2],).Seq', 'values';
    is $pair.kv.elems, 2, 'kv';
    is $pair.antipairs.raku, '([1, 2] => ("k", "v"),).Seq', 'antipairs';
}

subtest 'results are Seqs', {
    plan 6;
    my $pair = a => 5;
    for <keys values kv pairs antipairs invert> -> $method {
        is $pair."$method"().^name, 'Seq', ".$method is a Seq";
    }
}
