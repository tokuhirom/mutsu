use Test;

# `|$quanthash` is `$quanthash.Slip`, i.e. its `.list` of Pairs, as positional
# items. It used to slip the QuantHash itself as one item, so
# `%b = |$counts.BagHash` (CRDT) refilled a BagHash from a single Bag.

plan 6;

sub f(*@pos, *%named) { @pos.map(*.key).sort.join(','), %named.elems }

is-deeply f(|<a b>.Set), ('a,b', 0), 'a Set slips its pairs, positionally';
is-deeply f(|<a a b>.Bag), ('a,b', 0), 'a Bag too';

my @list = |<x y>.SetHash;
is @list.elems, 2, 'into a list, one item per element';
is-deeply @list.map(*.key).sort.List, <x y>, 'each a Pair';

my %b is BagHash = <a>;
%b = |<p q q>.BagHash;
is %b<q>, 2, 'refilling a BagHash from a slipped BagHash keeps the weights';
is (|<a b c>.Mix).elems, 3, 'a Mix slips its pairs';
