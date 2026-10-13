use Test;

# `flat($levels, :hammer)` is `Iterable`'s (and `Array`'s) row of the method
# table (ADR-11276 §9.104); `Any` and `Range` declare only `flat()`.

plan 20;

my $nested = (1, (2, (3, (4,))));
is-deeply $nested.flat(0), (1, (2, (3, (4,)))).Seq, 'flat(0) flattens nothing';
is-deeply $nested.flat(1), (1, 2, (3, (4,))).Seq, 'flat(1) flattens one level';
is-deeply $nested.flat(2), (1, 2, 3, (4,)).Seq, 'flat(2) flattens two levels';
is-deeply $nested.flat(-1), (1, 2, 3, 4).Seq, 'a negative level flattens every level';
is-deeply $nested.flat(Inf), (1, 2, 3, 4).Seq, 'Inf flattens every level';
is-deeply $nested.flat(*), (1, 2, 3, 4).Seq, 'Whatever flattens every level';
is-deeply $nested.flat(1.5), (1, 2, (3, (4,))).Seq, 'a Rat level is truncated';
is-deeply $nested.flat("2"), (1, 2, 3, (4,)).Seq, 'a Str level numifies';
dies-ok { $nested.flat("x") }, 'a non-numeric Str level dies';
is-deeply $nested.flat(1, :hammer), (1, 2, (3, (4,))).Seq, 'a level with :hammer';

my $array = [1, [2, [3]]];
is $array.flat(1).elems, 2, 'an Array\'s elements are items: the level changes nothing';
is $array.flat("x").elems, 2, 'an Array without :hammer never reads the level';
is-deeply (1, [2, [3]]).flat(1, :hammer), (1, 2, $[3]).Seq, ':hammer descends into arrays';

is-deeply { a => 1 }.flat(1), (:a(1),).Seq, 'a Hash flattens as its pairs';
is-deeply { a => 1 }.flat(:hammer), (:a(1),).Seq, 'flat(:hammer) of a Hash is its pairs';
is-deeply (1, (2, 3)).Seq.flat(2), (1, 2, 3).Seq, 'a Seq';
is-deeply (1, [2, [3]]).Slip.flat(1, :hammer), (1, 2, $[3]).Seq, 'a Slip';

dies-ok { 5.flat(1) }, 'Any declares no flat($levels)';
dies-ok { "ab".flat(1) }, 'nor does a Str';
dies-ok { (1..3).flat(1) }, 'nor does a Range';
