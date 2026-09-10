use v6;
use Test;

plan 8;

# A trailing comma in a `take` argument must be ignored: `take X,` == `take X`.
# It must NOT wrap the value in a 1-element list.

# scalar value
{
    my @r = gather { take 5, };
    is @r.elems, 1, 'take 5, yields one element';
    is @r[0], 5, 'take 5, yields the scalar 5 (not a list)';
}

# array value
{
    my @r = gather { take [1, 2], };
    is @r.elems, 1, 'take [1,2], yields one element';
    is-deeply @r[0], [1, 2], 'take [1,2], yields the array itself';
}

# parenthesized list value
{
    my @r = gather { take (5, 6), };
    is @r.elems, 1, 'take (5,6), yields one element';
    is-deeply @r[0], (5, 6), 'take (5,6), yields the list itself';
}

# without trailing comma behaves identically
{
    my @r = gather { take 42 };
    is @r.elems, 1, 'take 42 yields one element';
    is @r[0], 42, 'take 42 yields the scalar 42';
}
