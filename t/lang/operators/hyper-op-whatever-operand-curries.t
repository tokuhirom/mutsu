use Test;

plan 9;

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

# Non-Whatever hyper operators are unchanged.
is-deeply (1, 2, 3) »+» 1, (2, 3, 4), 'plain hyper operator';
is-deeply <a b> »~» 'x', <ax bx>, 'plain hyper concatenation';
