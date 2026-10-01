use Test;

# #10370: assigning a whole array variable to a RANGE slice of a native array
# (`@arr[2 ..^ 4] = @o`) is a distributing slice assignment. The compiler used
# to mistake the range index (a single `Binary` node) for a single-element
# subscript and route it through the element-share path, which reached the
# native-array bind guard and died with "Cannot bind to a native int array".

plan 9;

{
    my int32 @arr = 0 xx 10;
    my @o = "hi".ords;
    @arr[2 ..^ 2 + @o.elems] = @o;
    is @arr[0..4].raku, 'array[int32].new(0, 0, 104, 105, 0)', 'int32 array ..^ range slice = @array';
}

{
    my int @a = 0 xx 5;
    my @o = 7, 8;
    @a[1..2] = @o;
    is-deeply @a.List, (0, 7, 8, 0, 0), 'int array .. range slice = @array';
}

{
    my int @a = 0 xx 5;
    my @o = 7, 8;
    @a[^2] = @o;
    is-deeply @a.List, (7, 8, 0, 0, 0), 'int array ^N slice = @array';
}

{
    my int @a = 0 xx 5;
    my @o = 7, 8;
    @a[1 ^..^ 4] = @o;
    is-deeply @a.List, (0, 0, 7, 8, 0), 'int array ^..^ range slice = @array';
}

{
    my num @a = 0e0 xx 4;
    my @o = 1e0, 2e0;
    @a[1..2] = @o;
    is-deeply @a.List, (0e0, 1e0, 2e0, 0e0), 'num array range slice = @array';
}

{
    my str @a = '' xx 3;
    my @o = <x y>;
    @a[0..1] = @o;
    is-deeply @a.List, ('x', 'y', ''), 'str array range slice = @array';
}

{
    my int @a = 0 xx 4;
    my @o = 5, 6;
    @a[1 xx 2] = @o;
    is-deeply @a.List, (0, 6, 0, 0), 'int array xx-index slice = @array';
}

{
    my @a = 0 xx 5;
    my @o = 7, 8;
    @a[1..2] = @o;
    @o[0] = 99;
    is-deeply @a.List, (0, 7, 8, 0, 0), 'plain array range slice copies values, no share';
}

# A single computed index still shares the array by reference (Slice 2b).
{
    my @row = 1, 2;
    my @aoa;
    @aoa[0 + 1] = @row;
    @row.push(3);
    is-deeply @aoa[1].List, (1, 2, 3), 'single computed index still shares the source array';
}
