use Test;

plan 8;

# WhateverCode ranges as subscript indices (multi-param WhateverCode)
{
    my @a = 'a', 'b', 'c', 'e';
    is ~@a[*-4 .. *-2], 'a b c', 'WhateverCode range as subscript: @a[*-4 .. *-2]';
}

{
    my @a = 'a', 'b', 'c', 'd';
    @a[*-4 .. *-1] = 'd', 'c', 'b', 'a';
    is ~@a, 'd c b a', 'WhateverCode range as lvalue subscript';
}

{
    my @a = 1, 2, 3, 4, 5;
    is ~@a[*-3 .. *-1], '3 4 5', 'WhateverCode range last 3 elements';
}

{
    my @a = 10, 20, 30, 40;
    my @slice = @a[*-4 .. *-2];
    is @slice.elems, 3, 'WhateverCode range slice has correct number of elements';
}

# Single-param WhateverCode still works
{
    my @a = 'x', 'y', 'z';
    is @a[*-1], 'z', 'single WhateverCode subscript still works';
}

{
    my @a = 1, 2, 3, 4, 5;
    is @a[*-1], 5, 'single WhateverCode *-1';
}

# Range with one WhateverCode endpoint
{
    my @a = 'a', 'b', 'c', 'd', 'e';
    is ~@a[0 .. *-2], 'a b c d', 'range with WhateverCode end: 0..*-2';
}

{
    my @a = 'a', 'b', 'c', 'd', 'e';
    is ~@a[*-3 .. 4], 'c d e', 'range with WhateverCode start: *-3..4';
}
