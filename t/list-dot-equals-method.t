use Test;

plan 6;

# `($x, $y) .= method` applies the method to the WHOLE parenthesized list and
# assigns the result back across the lvalues (`($x, $y) = ($x, $y).method`),
# not per-element.

{
    my ($x, $y) = 7, 35;
    ($x, $y) .= reverse;
    is-deeply [$x, $y], [35, 7], 'two-element list .= reverse';
}

{
    my ($a, $b, $c) = 1, 2, 3;
    ($a, $b, $c) .= reverse;
    is-deeply [$a, $b, $c], [3, 2, 1], 'three-element list .= reverse';
}

{
    my ($a, $b, $c) = 3, 1, 2;
    ($a, $b, $c) .= sort;
    is-deeply [$a, $b, $c], [1, 2, 3], 'list .= sort';
}

# Scalar `.=` is unaffected.
{
    my $s = 'abc';
    $s .= flip;
    is $s, 'cba', 'scalar .= flip still works';
}

# Array variable `.=` is unaffected.
{
    my @a = 3, 1, 2;
    @a .= sort;
    is-deeply @a, [1, 2, 3], 'array var .= sort still works';
}

# Element `.=` is unaffected.
{
    my @a = 'ab', 'cd';
    @a[0] .= flip;
    is-deeply @a, ['ba', 'cd'], 'array element .= flip still works';
}
