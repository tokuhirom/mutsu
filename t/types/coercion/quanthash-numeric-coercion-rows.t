use Test;

# #13281: `QuantHash` declares `Int`, `Numeric`, `Real` and `Num`, composed
# into every quant hash: the total, as the type each coercion names.

plan 15;

is-deeply set(1, 2).Numeric, 2, 'Set.Numeric is the number of elements';
is-deeply set(1, 2).Int, 2, 'Set.Int';
is-deeply set(1, 2).Real, 2, 'Set.Real';
is-deeply set(1, 2).Num, 2e0, 'Set.Num';
is-deeply set().Int, 0, 'an empty Set.Int';
is-deeply SetHash.new(1, 2, 3).Int, 3, 'SetHash.Int';
is-deeply bag(1, 1).Int, 2, 'Bag.Int is the total';
is-deeply bag(1, 1).Real, 2, 'Bag.Real';
is-deeply bag(1, 1, 2).Num, 3e0, 'Bag.Num';
is-deeply BagHash.new(1, 1).Numeric, 2, 'BagHash.Numeric';
my $m = (a => 0.5, b => 1).Mix;
is-deeply $m.Int, 1, 'Mix.Int truncates the total';
is-deeply $m.Numeric, 1.5, 'Mix.Numeric is the total';
is-deeply $m.Real, 1.5, 'Mix.Real';
is-deeply $m.Num, 1.5e0, 'Mix.Num';
is-deeply (a => -0.5).Mix.Int, 0, 'a negative total truncates toward zero';

