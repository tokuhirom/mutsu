use Test;

plan 12;

# Bag.sort yields a sorted Seq of `key => weight` pairs (not the Bag itself).
{
    my $b = (1, 1, 2, 3, 3, 3).Bag;
    is $b.sort.^name, 'Seq', 'Bag.sort returns a Seq';
    is $b.sort.map(*.gist).join(','), '1 => 2,2 => 1,3 => 3', 'Bag.sort is by key';
    is $b.sort».gist.join(','), '1 => 2,2 => 1,3 => 3', 'Bag.sort».gist works';
}

# Numeric keys sort numerically (typed keys preserved).
{
    my $b = bag(1, 2, 10, 10);
    is $b.sort.map(*.key).join(','), '1,2,10', 'numeric keys sort numerically';
    is $b.sort.map(*.key.^name).unique.join(','), 'Int', 'keys keep Int type';
}

# Set.sort yields `key => True` pairs sorted by key.
{
    my $s = set(<z a m>);
    is $s.sort.map(*.key).join(','), 'a,m,z', 'Set.sort sorts keys';
    is $s.sort.map(*.value).unique.join(','), 'True', 'Set.sort values are True';
}

# Mix.sort yields `key => weight` pairs sorted by key.
{
    my $m = ("b" => 1.5, "a" => 2.5).Mix;
    is $m.sort.map(*.key).join(','), 'a,b', 'Mix.sort sorts keys';
    is $m.sort.map(*.value).join(','), '2.5,1.5', 'Mix.sort weights follow keys';
}

# Sort with a comparator over the pairs.
{
    my $cb = (1, 1, 2, 3, 3, 3).Bag;
    is $cb.sort(*.value).map(*.key).join(','), '2,1,3', 'Bag.sort(*.value) by weight';
    is $cb.sort(-> $x, $y { $y.value <=> $x.value }).map(*.key).join(','), '3,1,2',
        'Bag.sort descending by weight';
}

# Sorting an empty Bag is an empty Seq.
is bag().sort.elems, 0, 'empty Bag.sort is empty';
