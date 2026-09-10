use Test;

plan 9;

ok Set ~~ Setty, 'Set type object matches Setty';
nok Bag ~~ Setty, 'Bag type object does not match Setty';

is Set.Baggy.^name, 'Bag', 'Set.Baggy returns Bag type object';
is SetHash.Baggy.^name, 'BagHash', 'SetHash.Baggy returns BagHash type object';
is SetHash.Mixy.^name, 'MixHash', 'SetHash.Mixy returns MixHash type object';
is Bag.Setty.^name, 'Set', 'Bag.Setty returns Set type object';

is-deeply Bag('a'), bag('a'), 'Bag(...) constructor call works';
is-deeply Set('a'), set('a'), 'Set(...) constructor call works';
is-deeply Mix('a'), mix('a'), 'Mix(...) constructor call works';
