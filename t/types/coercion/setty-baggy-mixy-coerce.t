use v6;
use Test;

# .Setty / .Baggy / .Mixy coerce a Set/Bag/Mix (and their *Hash flavors) to the
# corresponding quant-hash flavor, rather than returning the bare type object.
# See Type/QuantHash.rakudoc.

plan 20;

my %b is Bag = one => 1, two => 2;
is %b.Setty.gist, 'Set(one two)',          'Bag.Setty drops weights to a Set';
is %b.Baggy.gist, 'Bag(one two(2))',       'Bag.Baggy keeps weights';
is %b.Mixy.gist,  'Mix(one two(2))',       'Bag.Mixy keeps weights as a Mix';

my %s is Set = <one two>;
is %s.Setty.gist, 'Set(one two)',          'Set.Setty is a Set';
is %s.Baggy.gist, 'Bag(one two)',          'Set.Baggy weight-1 Bag';
is %s.Mixy.gist,  'Mix(one two)',          'Set.Mixy weight-1 Mix';

my %m is Mix = one => 1, minus => -1, zero => 0;
is %m.Setty.gist, 'Set(one)',              'Mix.Setty keeps only positive weights';
is %m.Baggy.gist, 'Bag(one)',              'Mix.Baggy keeps only positive weights';
is %m.Mixy.gist,  'Mix(minus(-1) one)',    'Mix.Mixy keeps non-zero weights';

# .Set on a Mix must also drop non-positive weights (regression).
is %m.Set.gist,   'Set(one)',              'Mix.Set drops non-positive weights';

# Hash flavors preserve hashiness.
my %sh is SetHash = <a b>;
is %sh.Setty.^name, 'SetHash',             'SetHash.Setty stays a SetHash';
is %sh.Baggy.^name, 'BagHash',             'SetHash.Baggy is a BagHash';
is %sh.Mixy.^name,  'MixHash',             'SetHash.Mixy is a MixHash';

my %bh is BagHash = a => 2;
is %bh.Setty.^name, 'SetHash',             'BagHash.Setty is a SetHash';
is %bh.Baggy.^name, 'BagHash',             'BagHash.Baggy stays a BagHash';

my %mh is MixHash = a => 2, b => -1;
is %mh.Setty.gist, 'SetHash(a)',           'MixHash.Setty drops non-positive';
is %mh.Baggy.gist, 'BagHash(a(2))',        'MixHash.Baggy drops non-positive';
is %mh.Mixy.^name, 'MixHash',              'MixHash.Mixy stays a MixHash';

# Type objects map to the mapped type object.
is Bag.Setty.^name, 'Set',                 'Bag type object .Setty is the Set type';
is Mix.Baggy.^name, 'Bag',                 'Mix type object .Baggy is the Bag type';

done-testing;
