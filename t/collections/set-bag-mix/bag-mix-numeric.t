use Test;

plan 8;

is bag(1,1,2).Numeric, 3, 'Bag.Numeric is the total weight';
is BagHash.new(<a b b>).Numeric, 3, 'BagHash.Numeric is the total weight';
is mix(1,2).Numeric, 2, 'Mix.Numeric is the total weight';
is (a => 1.5, b => 2).Mix.Numeric, 3.5, 'Mix.Numeric with fractional weights';
ok bag(1,1) == bag(2,2), 'Bags with equal totals are == numerically';
ok mix(1,2) == mix(3,4), 'Mixes with equal totals are == numerically';
nok bag(1,1) == bag(2), 'Bags with different totals are not ==';
ok bag(1,1,2) == 3, 'a Bag == an Int compares by total weight';
