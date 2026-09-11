use Test;

# `.STORE` on a mutable QuantHash re-initializes it from the assigned list.
# The entries it writes must use the same `.WHICH`-derived storage keys every
# other QuantHash write uses, or the container reads back empty.

plan 10;

my $bh = BagHash.new;
my $stored = $bh.STORE((a => 42, b => 666));
is $stored.^name, 'BagHash', 'STORE answers a BagHash';
is $stored<a>,    42,        'and its entries are readable by key';
is $stored<b>,    666,       'for every key';
is $stored.total, 708,       'with the assigned weights';

my $sh = SetHash.new;
my $set = $sh.STORE(('x', 'y', 'x'));
is $set.elems, 2,    'STORE on a SetHash folds duplicates';
is $set<x>,    True, 'and its members are readable';

my $mh = MixHash.new;
my $mix = $mh.STORE((pi => 3.14));
is $mix<pi>, 3.14, 'STORE on a MixHash keeps a fractional weight';

# Non-Str elements keep their identity through the store.
my $objs = BagHash.new;
my $bag = $objs.STORE((1, 1, 2));
is $bag{1}, 2, 'an Int element is found by its own key';
is $bag{2}, 1, 'and so is every other';
is $bag.keys.sort.join(','), '1,2', 'the keys decode back to the elements';

# vim: expandtab shiftwidth=4
