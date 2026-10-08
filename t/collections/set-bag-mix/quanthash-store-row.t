use Test;

# `STORE` of the six quant hashes is one row (ADR-11276 §9.40): a mutable
# one is re-initialized, an immutable one refuses, an `is BagHash` subclass
# reaches the same row through `nextsame`.

plan 8;

my $bh = BagHash.new(<a b>);
$bh.STORE(<x y y>);
is $bh.sort(*.key).map({ .key ~ .value }).join(','), 'x1,y2', 'BagHash.STORE replaces the contents';

my $sh = SetHash.new(<a b>);
$sh.STORE(<c d>);
is $sh.keys.sort.join(','), 'c,d', 'SetHash.STORE replaces the contents';

my $mh = MixHash.new(<a>);
$mh.STORE((a => 1.5, b => 2));
is $mh<a>, 1.5, 'MixHash.STORE keeps a pair weight';

throws-like { Bag.new(<a>).STORE(<b>) }, X::Assignment::RO, 'Bag.STORE refuses';
throws-like { Set.new(<a>).STORE(<b>) }, X::Assignment::RO, 'Set.STORE refuses';

class Counting is BagHash {
    method STORE(|c) { nextsame }
}
my $c = Counting.new(<a>);
$c.STORE(<p q q>);
is $c<q>, 2, 'nextsame from a subclass STORE lands on the row';
is $c<a>, 0, 'the old contents are gone';

my %b is BagHash = <m n n>;
is %b<n>, 2, 'a tied declaration stores through STORE';

