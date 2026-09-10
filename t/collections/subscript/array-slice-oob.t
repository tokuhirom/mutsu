use Test;
plan 14;

# An out-of-bounds ARRAY slice pads with Any (the vivifiable-container
# default raku reports), while a List/Seq slice pads with Nil
# (List.AT-POS returns Nil beyond the end). Verified against raku.
{
    my @a = 1, 2, 3;
    is-deeply @a[^5], (1, 2, 3, Any, Any),
        'exclusive range slice pads with Any';
    is-deeply @a[0..6], (1, 2, 3, Any, Any, Any, Any),
        'inclusive range slice pads with Any';
}

# Empty array slicing
{
    my @a;
    is-deeply @a[^3], (Any, Any, Any),
        'exclusive range slice on empty array pads with Any';
    is-deeply @a[0..2], (Any, Any, Any),
        'inclusive range slice on empty array pads with Any';
}

# Lazy slicing should clip at array boundary
{
    my @a = 1, 2, 3;
    is-deeply @a[lazy ^5], (1, 2, 3),
        'lazy exclusive range slice clips at boundary';
}

# Slicing with individual array indices
{
    my @a = 1, 2, 3;
    is-deeply @a[0, 1, 2, 3, 4], (1, 2, 3, Any, Any),
        'array index slice pads with Any';
}

# A LIST slice beyond the end pads with Nil, not Any
{
    is-deeply (1, 2, 3)[^5], (1, 2, 3, Nil, Nil),
        'list slice pads with Nil';
}

# Open-ended slices should clip at array boundary
{
    my @a = 1, 2, 3, 4;
    is-deeply @a[2..*], (3, 4), 'inclusive open-ended slice clips at boundary';
}

# Seq/List slicing pads with Nil
{
    is-deeply (1 .. -Inf).reverse[^5], (Nil, Nil, Nil, Nil, Nil),
        'reverse of empty range padded with Nil';
}

# Typed array defaults
{
    my Int @a = 1, 2, 3;
    is @a[^5].elems, 5, 'typed array slice has correct length';
}

# Normal in-bounds slicing still works
{
    my @a = 10, 20, 30, 40, 50;
    is-deeply @a[^3], (10, 20, 30), 'in-bounds exclusive range slice';
    is-deeply @a[1..3], (20, 30, 40), 'in-bounds inclusive range slice';
    is-deeply @a[2..4], (30, 40, 50), 'in-bounds inclusive range slice at end';
}

# Empty range returns empty
{
    my @a = 1, 2, 3;
    is-deeply @a[^0], (), 'empty exclusive range returns empty';
}
