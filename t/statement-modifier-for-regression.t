use Test;

plan 6;

{
    my $a = 0;
    -> $i { $a += $i } for 1..3;
    is $a, 6, 'pointy block in postfix for gets called per iteration';
}

{
    $_ = 42;
    my @trace;
    @trace.push: $_ for 2, 3;
    is $_, 42, 'postfix for restores outer $_';
}

{
    my @x = <x x x>;
    $_ = 'foo' for @x;
    is @x, <foo foo foo>, 'assigning to $_ in postfix for writes back to @array elements';
}

{
    is ($_ for $[1,2,3]).elems, 1, 'postfix for respects itemized iterables';
}

{
    my @y = <& a& &b>;
    s:g/\&/\\\&/ for @y;
    is @y, ('\&', 'a\&', '\&b'), 'subst in postfix for writes back correctly once per element';
}

{
    sub foo { my $s; ($s += $_ for 1..3) }
    is foo(), (6, 6, 6), 'postfix for expression keeps containerized assignment result';
}
