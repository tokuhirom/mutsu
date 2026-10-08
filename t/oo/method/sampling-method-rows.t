use Test;

# pick / roll / pickpairs are rows of the built-in method table
# (ADR-11276, remainder: the sampling methods). Every receiver kind answers
# through the one implementation, however the call arrives.

plan 59;

# List and Array
my @a = 1..5;
ok (1, 2, 3).pick ~~ 1..3, 'List.pick';
ok @a.pick ~~ 1..5, 'Array.pick';
is @a.pick(3).elems, 3, 'Array.pick(3) picks three';
is @a.pick(3).unique.elems, 3, 'Array.pick(3) is without replacement';
is @a.pick(*).sort.join, '12345', 'Array.pick(*) is a shuffle';
is @a.pick(10).elems, 5, 'Array.pick(10) stops at the end';
is @a.pick(2.7).elems, 2, 'Array.pick takes the integer part of a Num';
is @a.pick(Whatever).elems, 5, 'Array.pick(Whatever) is the shuffle too';
is @a.roll(4).elems, 4, 'Array.roll(4)';
ok @a.roll(20).all ~~ 1..5, 'Array.roll(20) rolls with replacement';
is @a.roll(*).head(7).elems, 7, 'Array.roll(*) is infinite';
ok @a.roll(*).is-lazy, 'Array.roll(*) is lazy';
nok ().pick.defined, 'an empty List picks Nil';

# Range
ok (1..10).pick ~~ 1..10, 'Range.pick';
is (1..10).pick(4).unique.elems, 4, 'Range.pick(4)';
is (1..10).pick(*).sort.join(','), (1..10).join(','), 'Range.pick(*)';
ok (1..10).roll(30).all ~~ 1..10, 'Range.roll(30)';
ok (1^..^10).pick ~~ 2..9, 'an exclusive Range';
nok (5..1).pick.defined, 'an empty Range picks Nil';
ok ('a'..'e').pick ~~ 'a'..'e', 'a string Range';
ok (2**70 .. 2**70 + 5).pick ~~ 2**70 .. 2**70 + 5, 'a big Range';

# Hash
my %h = a => 1, b => 2, c => 3;
ok %h.pick ~~ Pair, 'Hash.pick is a Pair';
is %h.pick(2).unique.elems, 2, 'Hash.pick(2)';
is %h.roll(5).elems, 5, 'Hash.roll(5)';
ok %h.roll.key ~~ <a b c>.any, 'Hash.roll';

# Set / SetHash
my $set = <a b c>.Set;
ok $set.pick ~~ <a b c>.any, 'Set.pick';
is $set.pick(2).unique.elems, 2, 'Set.pick(2)';
is $set.pick(*).sort.join, 'abc', 'Set.pick(*)';
is $set.roll(6).elems, 6, 'Set.roll(6)';
ok $set.pickpairs.value === True, 'Set.pickpairs';
is $set.pickpairs(2).elems, 2, 'Set.pickpairs(2)';
ok $set.pickpairs(*).all.value === True, 'Set.pickpairs(*)';
ok <a b>.SetHash.pick ~~ <a b>.any, 'SetHash.pick';

# Bag / BagHash
my $bag = (a => 3, b => 1).Bag;
ok $bag.pick ~~ <a b>.any, 'Bag.pick';
is $bag.pick(4).sort.join, 'aaab', 'Bag.pick(4) takes every element';
is $bag.pick(*).elems, 4, 'Bag.pick(*)';
is $bag.roll(10).elems, 10, 'Bag.roll(10)';
my $pairs = $bag.pickpairs(2).list;
is $pairs.map(*.key).sort.join, 'ab', 'Bag.pickpairs(2) picks both keys';
is $pairs.map(*.value).sum, 4, 'Bag.pickpairs keeps the counts';
ok $bag.pickpairs ~~ Pair, 'Bag.pickpairs';
is $bag.pick(* - 1).elems, 3, 'a WhateverCode count is applied to the total';
ok (a => 2).BagHash.roll ~~ 'a', 'BagHash.roll';

# Mix / MixHash
my $mix = (a => 2, b => 0.5).Mix;
ok $mix.roll ~~ <a b>.any, 'Mix.roll';
is $mix.roll(5).elems, 5, 'Mix.roll(5)';
is $mix.pickpairs(2).elems, 2, 'Mix.pickpairs(2)';
ok $mix.pickpairs ~~ Pair, 'Mix.pickpairs';
is $mix.pickpairs(*).map(*.value).sum, 2.5, 'Mix.pickpairs(*) keeps the weights';
throws-like { $mix.pick }, Exception, 'Mix.pick is refused';

# Seq and a shaped array have no dispatch shape: the cascade arms call the same code.
ok (1..3).Seq.pick ~~ 1..3, 'Seq.pick';
is (1..6).Seq.pick(3).unique.elems, 3, 'Seq.pick(3)';
ok (1..3).Seq.roll(5).all ~~ 1..3, 'Seq.roll(5)';
my @shaped[2;2];
@shaped[0;0] = 1; @shaped[0;1] = 2; @shaped[1;0] = 3; @shaped[1;1] = 4;
ok @shaped.pick ~~ 1..4, 'a shaped array picks a leaf';

# Pair and Str
ok (a => 1).pick ~~ Pair, 'Pair.pick';
is 'abc'.pick, 'abc', 'Str.pick is the string itself';
is 'abc'.roll(2).join(','), 'abc,abc', 'Str.roll(2)';

# Routine form and a method on an itemized receiver
ok pick(2, @a).elems == 2, 'the routine form';
ok $@a.pick ~~ 1..5, 'an itemized Array';

# A user method of the same name wins over the built-in row.
class Pickable { method pick($n = 1) { "user pick $n" } }
is Pickable.new.pick(3), 'user pick 3', 'a user class overrides pick';

# Rakudo declares these where the rows are
ok List.^can('pick') && Range.^can('roll') && Bag.^can('pickpairs'), '.^can sees the rows';
