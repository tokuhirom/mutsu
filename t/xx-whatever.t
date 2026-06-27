use Test;

# A bare `*` operand of the replication operators `x` / `xx` is the Whatever
# *value*, repeated literally — NOT a Whatever-curry point. So `* xx 2` is
# `(*, *)`, not a `WhateverCode`. mutsu used to curry the whole expression into a
# WhateverCode (so `(1,2,3).map(* xx 2)` quietly produced a Seq instead of
# throwing X::Cannot::Map, and `(* xx 3).elems` returned a WhateverCode).
# A compound operand like `(*+1) xx 2` IS a WhateverCode (repeated).

plan 9;

is (* xx 2).raku,  '(*, *).Seq',     '`* xx 2` is the Whatever value repeated, not a WhateverCode';
is (* xx 3).elems, 3,                'a postfix on `* xx N` evaluates the repetition first';
is (1 xx 3).raku,  '(1, 1, 1).Seq',  'plain `1 xx 3` unaffected';
is ("a" xx 2).raku, '("a", "a").Seq', 'plain string `xx` unaffected';
is (1 xx *).head(3).raku, '(1, 1, 1).Seq', '`1 xx *` (infinite repeat) still works';

# A compound Whatever operand still curries, then repeats the WhateverCode.
is ((*+1) xx 2).map({ $_(10) }).raku, '(11, 11).Seq',
    '`(*+1) xx 2` repeats a WhateverCode';

# `.map` with a non-Callable (a Seq of Whatevers) throws X::Cannot::Map.
throws-like '(1, 2, 3).map(* xx 2)', X::Cannot::Map,
    '.map(* xx 2) throws X::Cannot::Map (the arg is a Seq, not Callable)';

# String replication `* x N` DOES curry into a WhateverCode (unlike list `xx`).
ok (* x 2) ~~ Callable, '`* x 2` (string replication) still curries into a WhateverCode';

# Sanity: bare Whatever in an ordinary expression still curries.
ok (* + 1) ~~ Callable, 'bare `*` in arithmetic still curries into a WhateverCode';
