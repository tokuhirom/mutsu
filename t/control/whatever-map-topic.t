use v6;
use Test;

# A WhateverCode's `*` placeholder compiles to a `$_`-named topic param. An
# ordinary inner block created inside the WhateverCode body (e.g. the block
# argument to `.map`) must NOT inherit the WhateverCode's identity marker, or
# the `.map` loop mis-treats the inner block as a WhateverCode and holds `$_`
# at the outer topic (the whole list) instead of binding it to each element.

plan 8;

my $h = { :a(1) };

# 1) single-element list argument
{
    my @names = (*.map({ $_.^name }))(($h,));
    is @names[0], 'Hash', 'inner $_ in *.map is the element (single-elem list)';
}

# 2) two-element list argument
{
    my @names = (*.map({ $_.^name }))(($h, $h));
    is @names.elems, 2, 'two elements mapped';
    is @names[0], 'Hash', 'inner $_ is element (elem 0)';
    is @names[1], 'Hash', 'inner $_ is element (elem 1)';
}

# 3) actual value transformation through the inner $_
{
    my @doubled = (*.map({ $_ * 2 }))((10, 20, 30));
    is-deeply @doubled.List, (20, 40, 60), 'inner $_ arithmetic per element';
}

# 4) associative index on the mapped element (the zef Repository pattern)
{
    my @keys = (*.map({ $_<k> }))(({ :k(42) },));
    is @keys[0], 42, 'inner $_<k> indexes the element hash, not the list';
}

# 5) nested: WhateverCode inside WhateverCode still binds its own topic
{
    my @r = (*.map(*.map({ $_ + 1 })))(((1, 2), (3, 4)));
    is-deeply @r[0].List, (2, 3), 'nested map inner + outer topics independent';
}

# 6) grep parity (was already correct; guard against regression)
{
    my @g = (*.grep({ $_ > 1 }))((0, 1, 2, 3));
    is-deeply @g.List, (2, 3), 'inner $_ in *.grep is the element';
}
