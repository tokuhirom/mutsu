use Test;

plan 10;

# Prefix + on WhateverCode should produce a new WhateverCode
{
    my $f = +("a" le *);
    is $f("b"), 1, '+("a" le *) with "b" gives 1';
    is $f("0"), 0, '+("a" le *) with "0" gives 0';
}

# Prefix - on WhateverCode
{
    my $f = -(* + 1);
    is $f(5), -6, '-(* + 1) with 5 gives -6';
    is $f(0), -1, '-(* + 1) with 0 gives -1';
}

# Prefix ~ on WhateverCode
{
    my $f = ~(* + 1);
    is $f(5), "6", '~(* + 1) with 5 gives "6"';
}

# Prefix ? on WhateverCode
{
    my $f = ?(* > 3);
    is $f(5), True, '?(* > 3) with 5 gives True';
    is $f(1), False, '?(* > 3) with 1 gives False';
}

# Using prefix + WhateverCode in map
{
    my @result = ("a" xx 3).map(+("a" le *));
    is @result.elems, 3, 'map with +("a" le *) returns 3 elements';
    is @result[0], 1, 'first element is 1';
}

# Prefix + on bare * should still be a WhateverCode (numify)
{
    my $f = +*;
    is $f("42"), 42, '+* numifies string to number';
}
