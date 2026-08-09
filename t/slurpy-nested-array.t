use Test;

plan 8;

sub f(*@c) { @c.elems }

is f([[1,2],[3,4]]), 2, 'a bare array-literal argument flattens one level only';

{
    my @x = [1,2],[3,4];
    is f(@x), 2, 'an @-array of arrays flattens one level only';
}

is f([$[1,2],$[3,4]]), 2, 'explicitly itemized elements are unaffected (already correct)';

is f([1,2]), 2, 'a plain array of scalars still flattens fully (unaffected)';

{
    my $a = [[1,2],[3,4]];
    is f($a), 1, 'a scalar-bound array argument stays a single element (unaffected)';
}

{
    my @x = 1,2;
    is f(@x), 2, 'a plain @-array of scalars still flattens fully (unaffected)';
}

is f((1,(2,3))), 3, 'a List of Lists still flattens fully (List recursion preserved)';

is f([1,[2,3]]), 2, 'an array literal mixing a scalar and a nested array flattens one level only';

done-testing;
