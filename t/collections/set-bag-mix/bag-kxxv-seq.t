use Test;

# `Baggy.kxxv` answers a Seq, like the other Baggy listings (measured against
# rakudo 2026.09).

plan 5;

is bag(<a b a>).kxxv.WHAT.^name, 'Seq', 'Bag.kxxv is a Seq';
is bag(<a b a>).kxxv.sort.join(','), 'a,a,b', 'with each element repeated by its count';
is <a b a>.BagHash.kxxv.WHAT.^name, 'Seq', 'BagHash.kxxv is a Seq';
my $x = bag(<a a>).kxxv;
is $x.raku, '$(("a", "a").Seq)', 'a stored one renders itemized';
is bag().kxxv.elems, 0, 'an empty Bag lists nothing';
