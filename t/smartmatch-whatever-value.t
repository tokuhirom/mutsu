use v6;
use Test;

# A Whatever *value* on the RHS of `~~` smartmatches to True (`X ~~ *` is always
# True per Whatever's ACCEPTS). A *syntactic* bare `*` autoprimes to a
# WhateverCode. mutsu previously autoprimed a Whatever value at runtime, so a
# parenthesized `(*)` or a variable holding a Whatever wrongly produced a
# WhateverCode instead of True. (roast/S02-types/whatever.t
# "Mu:U smartmatches as True with Whatever".)

plan 9;

# Parenthesized `(*)` is a Whatever value -> smartmatch is True.
is-deeply (Mu ~~ (*)), True, 'Mu ~~ (*) is True';
is-deeply (42 ~~ (*)), True, 'Int ~~ (*) is True';
is-deeply ("x" ~~ (*)), True, 'Str ~~ (*) is True';

# A variable holding a Whatever value -> smartmatch is True.
{
    my $w = *;
    is-deeply (42 ~~ $w), True, 'value ~~ $whatever-var is True';
    is-deeply (Mu ~~ $w), True, 'Mu ~~ $whatever-var is True';
}

# A *bare* `*` on the RHS still autoprimes to a WhateverCode.
ok (42 ~~ *) ~~ WhateverCode, 'bare X ~~ * autoprimes to a WhateverCode';
is (42 ~~ *)(42), True, 'the autoprimed WhateverCode smartmatches its arg';

# A WhateverCode RHS is still invoked (not treated as a Whatever value).
is-deeply (5 ~~ (* > 3)), True, 'X ~~ (WhateverCode) invokes it (true)';
is-deeply (2 ~~ (* > 3)), False, 'X ~~ (WhateverCode) invokes it (false)';
