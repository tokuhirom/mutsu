use Test;

# ADR-11276 slice 3C: `hyper`, `race`, `lazy`, `item` and `is-lazy` of the
# collections are handler rows (List, Map and Range own them; Array reaches
# List's and Hash Map's). Every answer below was checked against Rakudo.

plan 5;

subtest 'hyper and race', {
    plan 8;
    for (1, 2, 3), [4, 5], (1..5), %(a => 1) -> $collection {
        isa-ok $collection.hyper, HyperSeq, "{$collection.^name}.hyper";
        isa-ok $collection.race, RaceSeq, "{$collection.^name}.race";
    }
}

subtest 'the elements survive hyper and race', {
    plan 4;
    is (1, 2, 3).hyper.list.raku, '(1, 2, 3)', 'List.hyper';
    is (1, 2, 3).race.list.raku, '(1, 2, 3)', 'List.race';
    is (1..5).hyper.sum, 15, 'Range.hyper';
    is [1, 2, 3].race.map(* * 2).sort.join(' '), '2 4 6', 'Array.race';
}

subtest 'lazy', {
    plan 8;
    is (1, 2, 3).lazy.^name, 'Seq', 'a List becomes a Seq';
    is [4, 5].lazy.^name, 'Seq', 'an Array becomes a Seq';
    is (1..5).lazy.^name, 'Seq', 'a finite Range becomes a Seq';
    is (1..*).lazy.^name, 'Seq', 'an unbounded Range becomes a Seq';
    ok (1..*).lazy.is-lazy, 'which is lazy';
    ok (1..3).lazy.is-lazy, 'a lazy Range is lazy';
    is (1..3).lazy.eager.join(' '), '1 2 3', 'and keeps its elements';
    my @a = (1..3).lazy;
    ok @a.is-lazy, 'assigning a lazy Range to an array keeps it lazy';
}

subtest 'item', {
    plan 4;
    is (1..3).item.^name, 'Range', 'a Range stays a Range';
    is %(a => 1).item.^name, 'Hash', 'a Hash stays a Hash';
    is-deeply (1, 2).item.elems, 2, 'an itemized List keeps its elements';
    is [1, [2, 3]].item[1].elems, 2, 'an element of an itemized Array';
}

subtest 'is-lazy of a Range', {
    plan 5;
    nok (1..3).is-lazy, 'a closed Range';
    ok (1..*).is-lazy, 'an open end';
    ok (1..Inf).is-lazy, 'an Inf end';
    ok (-Inf..1).is-lazy, 'an -Inf start';
    nok ('a'..'c').is-lazy, 'a string Range';
}
