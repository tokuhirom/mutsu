use Test;

# A list-assignment to an array binding that has no *declared* element type of
# its own, but currently holds an element-typed container (`array[int]`), writes
# INTO that container and preserves its element type, rather than replacing it
# with an untyped `Array`. Previously the assignment dropped the element type.

plan 6;

# for-loop binding over typed arrays (the S32-array/splice.t pattern).
{
    my int @int = 1, 2, 3;
    my @testing = $@int, array[int];
    for @testing -> @a, $T {
        @a = 5, 6, 7, 8;
        is @a.WHAT.^name, 'array[int]',
            'for-loop bound typed array keeps element type after reassignment';
        is $T.^name, 'array[int]', 'expected type object name';
    }
}

# splice return/remainder of a for-bound native int array stays typed.
{
    my int @int = 1 .. 10;
    my @testing = $@int, array[int];
    for @testing -> @a, $T {
        @a = 1 .. 10;
        my $ret = splice(@a, 0, 2);
        is $ret.WHAT.^name, 'array[int]', 'splice return keeps element type';
        is @a.WHAT.^name, 'array[int]', 'remainder keeps element type';
    }
}

# A plain untyped array reassignment must NOT gain a spurious type.
{
    my @plain = 1, 2, 3;
    @plain = 4, 5, 6;
    is @plain.WHAT.^name, 'Array', 'untyped array stays untyped after reassignment';
}

# A directly-declared typed array still keeps its type (unchanged behavior).
{
    my int @d = 1, 2, 3;
    @d = 9, 8, 7;
    is @d.WHAT.^name, 'array[int]', 'directly-declared typed array keeps type';
}
