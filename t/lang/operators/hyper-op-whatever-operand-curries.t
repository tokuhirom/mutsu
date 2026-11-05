use Test;

plan 10;

# A hyper operator is not Whatever-aware: a bare `*` operand makes the whole
# expression a WhateverCode, like any other infix (roast
# 6.c/MISC/bug-coverage-stress.t, rakudo/rakudo#2120).
my $c = (14, 12, 12) »%» *;
isa-ok $c, WhateverCode, '(list) »%» * is a WhateverCode';
is-deeply $c(10), (4, 2, 2), 'calling it applies the hyper operator';

my $d = * «+« (1, 2);
isa-ok $d, WhateverCode, '* «+« (list) is a WhateverCode';
is-deeply $d(10), (11, 12), 'the Whatever is the left operand';

is-deeply ((1, 2) »+» *)(10), (11, 12), 'immediately invoked';
is-deeply (1..10)[(4, 2, 2, 2) »%» *], (5, 3, 3, 3), 'as a subscript it gets the element count';
is-deeply (1..10)[(1, 2) »+» *], (Nil, Nil), 'indices past the end';

# An infinite repeat `0 xx *` inside a hyper operator's operand is a repeat,
# not a priming point.
is-deeply ((0 xx *) <<+<< (1..3)).list, (1, 2, 3), 'infinite left cycles up to finite right';

# Non-Whatever hyper operators are unchanged.
is-deeply (1, 2, 3) »+» 1, (2, 3, 4), 'plain hyper operator';
is-deeply <a b> »~» 'x', <ax bx>, 'plain hyper concatenation';
