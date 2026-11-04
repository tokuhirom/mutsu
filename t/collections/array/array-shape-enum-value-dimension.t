use Test;
plan 5;

enum Cards <Trump Ace Deuce Trey>;

# An enum VALUE used as a shape dimension is its numeric value; an enum TYPE
# object counts its variants.
{
    my @a[Deuce;Deuce];
    is @a.shape.raku, '(2, 2)', 'enum values as dimensions use their numeric value';
}
{
    my @b[Ace];
    is @b.shape.raku, '(1,)', 'a one-dimensional shape from an enum value';
}
{
    my @c[Cards];
    is @c.shape.raku, '(4,)', 'an enum type object counts its variants';
}
{
    my @d[Trey; 2];
    is @d.shape.raku, '(3, 2)', 'enum value mixed with an Int';
}
throws-like { EVAL 'my @z[Trump]' }, X::IllegalDimensionInShape, 'a zero-valued enum is an illegal dimension';
