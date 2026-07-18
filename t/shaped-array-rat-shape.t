use Test;

plan 8;

# A non-integer numeric shape dimension coerces toward Int (truncation),
# matching raku: `my Int @a[2.5]` is a 2-element shaped array.
# https://github.com/rakudo/rakudo/issues/1794 (S02-types/array-shapes.t)

{
    my Int @a[2.5];
    is @a.gist, "[(Int) (Int)]", 'my Int @a[2.5] is a 2-element shaped array';
    is @a.shape.raku, "(2,)", '.shape is (2) after Rat->Int truncation';
    is @a.elems, 2, '.elems is 2';
}

{
    my @a[3.9];
    is @a.elems, 3, '3.9 truncates to 3';
}

{
    my @a[2.5; 3.1];
    is @a.shape.raku, "(2, 3)", 'multi-dim Rat shape truncates each dimension';
}

# A rational that truncates to a non-positive value is still illegal.
throws-like { my @a[-2.5] }, X::IllegalDimensionInShape,
    'negative Rat dimension is illegal';
throws-like { my @a[0.5] }, X::IllegalDimensionInShape,
    'Rat dimension truncating to 0 is illegal';

# Integer shapes are unaffected.
{
    my @a[2; 3];
    is @a.shape.raku, "(2, 3)", 'integer shape unchanged';
}
