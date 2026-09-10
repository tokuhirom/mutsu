use Test;

plan 7;

# `$(EXPR)` is the item contextualizer; as an lvalue it is transparent and
# names the same container as EXPR, so `$(@a[0]) = ...` writes through to
# `@a[0]`. It used to lower to a `.item` method-lvalue, which had no writable
# slot and died with X::Assignment::RO ("cannot assign through .item ...").

{
    my @a;
    $(@a[0]) = 5;
    is @a[0], 5, '$(@a[0]) = scalar writes through to the element';
}

{
    sub l { (1, 2) }
    my @a;
    # Item assignment binds tighter than the comma: `($(@a[0]) = l), l`.
    my @z = ($(@a[0]) = l, l);
    is @a[0].elems, 2, '$(@a[0]) as scalar holds the whole list (elems)';
    is @z.elems,    2, '... and the outer list keeps two items';
}

{
    my $s;
    $($s) = 42;
    is $s, 42, '$($scalar) = value writes through to the scalar';
}

{
    my %h;
    $(%h<k>) = 9;
    is %h<k>, 9, '$(%h<k>) = value writes through to the hash element';
}

{
    # In-place update through the item deref.
    my @a = 10, 20, 30;
    $(@a[1]) = 99;
    is @a[1], 99, '$(@a[1]) = value overwrites an existing element';
    is @a.elems, 3, '... without disturbing the array shape';
}
