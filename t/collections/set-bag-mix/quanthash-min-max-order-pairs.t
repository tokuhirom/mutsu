use v6;
use Test;

plan 14;

# A Set/Bag/Mix is a collection of `key => weight` pairs: its extremum is the
# largest / smallest Pair (compared as Pairs: key first), not the collection.
my $bag = bag <a a b c c c>;
is $bag.max.key, 'c', 'Bag.max is the Pair with the largest key';
is $bag.max.value, 3, 'with its weight';
is $bag.min.key, 'a', 'Bag.min is the Pair with the smallest key';
isa-ok $bag.max, Pair, 'a Pair';

my $set = set <x y z>;
is $set.max.key, 'z', 'Set.max';
is $set.max.value, True, 'a Set weight is True';
is $set.min.key, 'x', 'Set.min';

is (set 5, 20, 3).max.key, 20, 'numeric keys compare numerically';

my $mix = (a => 2.5, b => 0.5).Mix;
is $mix.max.key, 'b', 'Mix.max';
is $mix.min.value, 2.5, 'Mix.min';

is SetHash.new(<q r>).max.key, 'r', 'a mutable twin';
is BagHash.new(<q q r>).min.value, 2, 'BagHash.min';

# Native::FindVersion's shape: union strings into a Set, take the newest.
my Set $versions;
$versions ∪= $_ for <1.2 3.4 2.0>;
is $versions.max.key, '3.4', 'the newest version string via Set.max.key';
is Version.new($versions.max.key), v3.4, 'which makes a Version';
