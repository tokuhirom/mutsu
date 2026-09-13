use v6;
use Test;

# A `WhateverCode` dimension (`*-1`) in a multi-dimensional subscript makes
# rakudo classify the dimension as a slice of one, returning a one-element
# List -- the same as a Range dimension that happens to select one element
# (`@m[0..0; 0]` is `(1)`), NOT the bare element a plain Int dimension gives
# (`@m[1; 0]` is `4`). mutsu resolved the WhateverCode to a plain Int index
# and then took the scalar path, losing the List wrapper.
#
# It is the VALUE's shape that decides this, not the subscript's literal
# syntax: a variable holding a WhateverCode still slices (`my $i = *-1; @m[$i;
# 0]` is `(4)`), and so does an ordinary block dimension (`@m[{0}; 0]` is
# `(1)`) -- both go through the same runtime representation as `*-1`.
#
# Every expectation below was measured against rakudo (#8188), under the
# default `v6.d` this file implicitly runs as. 6.e reclassifies a
# WhateverCode/block dimension back to a plain scalar index (the same
# exception the associative subscript already has) -- that side is pinned
# separately by `roast/S32-array/multislice-6e.t` under `use v6.e.PREVIEW`.

plan 18;

my @m = [[1, 2, 3], [4, 5, 6]];

# The four dimension kinds, bracket spelling.
is-deeply @m[*-1; 0], (4,), 'a WhateverCode dimension wraps the leaf in a List';
is @m[*-1; 0].^name, 'List', 'and its type is List, not Int';
is-deeply @m[1; *-1], (6,), 'a WhateverCode in the second dimension also wraps';
is-deeply @m[1; 0], 4, 'a plain Int dimension stays a bare scalar';
is-deeply @m[0+0; 0], 1, 'a numeric-expression Int dimension stays scalar too';
is-deeply @m[0..0; 0], (1,), 'a Range dimension (already a slice) is unaffected';

# The dotted spelling goes through the same lowering (#8155) and must agree.
is-deeply @m.[*-1; 0], (4,), 'the dotted spelling wraps a WhateverCode dimension too';

# Classification follows the VALUE, not literal `*` syntax.
{
    my $i = *-1;
    is-deeply @m[$i; 0], (4,), 'a WhateverCode held in a variable still wraps';
}
{
    my $j = 0 + 0;
    is-deeply @m[$j; 0], 1, 'a plain Int held in a variable stays scalar';
}

# An ordinary block dimension shares the WhateverCode runtime representation
# and slices the same way, even for a single-index block.
is-deeply @m[{0}; 0], (1,), 'a block dimension also wraps its single result';

# Two WhateverCode dimensions at once: the result is one flat List, not a
# nested List of Lists.
is-deeply @m[*-1; *-1], (6,), 'two WhateverCode dimensions flatten to one List';

# A WhateverCode dimension alongside a Range dimension: still one flat List.
{
    my @n = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    is-deeply @n[*-1; 1..2], (8, 9), 'WhateverCode + Range flattens to one List';
}

# Three dimensions, WhateverCode in the middle.
{
    my @d = [[[7, 8], [9, 10]], [[11, 12], [13, 14]]];
    is-deeply @d[0; *-1; 0], (9,), 'a WhateverCode in a middle dimension wraps';
}

# Out-of-range WhateverCode resolution still wraps the resulting `Any`.
{
    my @e = [[1, 2], [3, 4]];
    is-deeply @e[*+5; 0], (Any,), 'an out-of-range WhateverCode dimension still wraps';
}

# It is an rvalue subscript classification only -- assignment through it keeps
# working as a slice-distributing target (already correct before this fix).
{
    my @w = [[1, 2, 3], [4, 5, 6]];
    @w[*-1; 0] = 99;
    is-deeply @w, [[1, 2, 3], [99, 5, 6]], 'assignment through a WhateverCode dimension is unaffected';
}

# Single-dimension WhateverCode subscript (not multi-dim) is untouched by this
# fix -- it stays a plain scalar, matching rakudo.
is @m[*-1].gist, '[4 5 6]', 'a single-dimension WhateverCode subscript stays a plain element';

# `.^name` on every wrapped case is `List`, never `Array`.
is @m[*-1; *-1].^name, 'List', 'flattened multi-WhateverCode result is a List';
is @m[{0}; 0].^name, 'List', 'block-dimension result is a List';

done-testing;
