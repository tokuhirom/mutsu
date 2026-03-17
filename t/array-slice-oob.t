use Test;
plan 13;

# Array slicing beyond bounds should pad with Nil
{
    my @a = 1, 2, 3;
    is-deeply @a[^5], (1, 2, 3, Nil, Nil),
        'exclusive range slice pads with Nil';
    is-deeply @a[0..6], (1, 2, 3, Nil, Nil, Nil, Nil),
        'inclusive range slice pads with Nil';
}

# Empty array slicing
{
    my @a;
    is-deeply @a[^3], (Nil, Nil, Nil),
        'exclusive range slice on empty array pads with Nil';
    is-deeply @a[0..2], (Nil, Nil, Nil),
        'inclusive range slice on empty array pads with Nil';
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
    is-deeply @a[0, 1, 2, 3, 4], (1, 2, 3, Nil, Nil),
        'array index slice pads with Nil';
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
