use Test;

# `my @a := [[1,2],[3,4]]` binds an ordinary (unshaped) nested Array. mutsu
# used to infer a "declared shape" from its first rows and record it under a
# name-keyed env marker, so a multi-dimensional store past those rows died
# with "Index out of bounds". A shape is a property of the array value
# (ArrayData.shape), never of the name it is bound to (ADR-0097).

plan 9;

{
    my @a := [[1, 2], [3, 4]];
    lives-ok { @a[5;0] = 1 }, 'a bound nested list grows on a store past its rows';
    is @a.elems, 6, 'the outer array grew to the new row';
    is @a[5][0], 1, 'the stored element is readable';
}

{
    my @b := [[1, 2], [3, 4]];
    @b = 7, 8, 9;
    is @b.raku, '[7, 8, 9]', 'whole assignment to a bound nested list is a flat store';
}

{
    my @c := [[1, 2], [3, 4]];
    @c[0;1] = 9;
    is @c.raku, '[[1, 9], [3, 4]]', 'an in-range multi-dim store writes the element';
}

{
    my @s[2;2];
    my @d := @s;
    @d[1;1] = 5;
    is @d.shape.join(','), '2,2', 'a bound shaped array keeps its shape';
    is @s[1;1], 5, 'a store through the binding reaches the shaped array';
    dies-ok { @d[3;0] = 1 }, 'a bound shaped array still refuses an out-of-range index';
}

{
    my @e := Array.new(:shape(2, 3));
    dies-ok { @e[5;0] = 1 }, 'a bound Array.new(:shape) refuses an out-of-range index';
}
