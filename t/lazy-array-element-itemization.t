use Test;

# ADR-0040: a real `Array` stores every element in a container, so an element
# that holds a list reads back itemized. A LAZY source assigned to an `@`
# variable reaches none of the store-side hooks -- the assignment stored ONE
# `LazyList` value, and the elements are first materialized by the force -- so
# the force itself has to itemize them. The SAME `LazyList` shape backs a bare
# lazy `Seq`, whose elements are values and must stay bare; `array_context` is
# what tells the two apart.

plan 14;

{
    my @a = lazy gather { take $_ for 1, (2, 3) };
    is @a[1].raku,        '$(2, 3)', 'a lazy-gather array element is itemized';
    is @a[0].raku,        '1',       'a scalar element is unchanged';
    is @a[1].VAR.^name,   'Scalar',  'and reflects as a Scalar container';
}
{
    my @a = lazy gather { take $_ for 1, (2, 3) };
    my @b = @a;
    is @b[1].raku, '$(2, 3)', 'copying the forced array keeps the itemization';
}
{
    my @a = lazy gather { take $_ for 1, (2, 3) };
    is @a[0, 1].raku, '(1, $(2, 3))', 'a slice hands out the itemized elements';
}
{
    my @a = lazy gather { take $_ for 1, (2, 3) };
    my @seen;
    for @a -> $e { @seen.push($e.raku) }
    is @seen.join('|'), '1|$(2, 3)', 'iterating sees the itemized elements';
}
{
    # A bare lazy Seq is NOT an array: its elements are the values themselves.
    my $s = lazy gather { take $_ for 1, (2, 3) };
    is $s[1].raku,      '(2, 3)', 'a bare lazy Seq element stays bare';
    is $s[1].VAR.^name, 'List',   'and .VAR on it is identity';
}
{
    my @a = lazy gather { take $_ for 1, (2, 3) };
    my @i = 0, 1;
    is @a[*].VAR.^name,    'List', 'a whatever slice of a lazy array is a List';
    is @a[@i].VAR.^name,   'List', 'a variable slice of a lazy array is a List';
    is @a[0..1].VAR.^name, 'List', 'a range slice of a lazy array is a List';
    is @a[1].VAR.^name,    'Scalar', 'a single subscript is still an element';
}
{
    # The eager sources already itemized at the store; they must keep doing so.
    my @sq = (1, (2, 3)).Seq;
    is @sq[1].raku, '$(2, 3)', 'an @-assigned Seq element is still itemized';
    my @m = (1, (2, 3)).map(*.self);
    is @m[1].raku,  '$(2, 3)', 'an @-assigned map pipe element is still itemized';
}
