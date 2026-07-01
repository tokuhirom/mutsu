use v6;
use Test;

# Only the *target* of an invocation curries into a WhateverCode. A Whatever
# passed as a call *argument* (`$sub(*)`, `&infix:<+>(*, 42)`) is a Whatever
# VALUE handed to the callee, not a placeholder of the enclosing WhateverCode,
# so it must not add to the WhateverCode's arity. mutsu previously counted such
# argument `*`s as placeholders (`(* + $b(*) + *.flip)` wanted 3 args, not 2).
# (roast/S02-types/whatever.t "various wild cases".)

plan 5;

my $b = sub (Whatever) { 100 };

# `$b(*)` passes Whatever to $b (returns 100); the outer WhateverCode has 2
# placeholders (`*` and `*.flip`), taking 2 args.
is (* + $b(*) + *.flip)(5, 13), 5 + 100 + 31, 'call-arg Whatever is a value, not a placeholder';

# A single placeholder plus a call-arg Whatever -> 1-arg WhateverCode.
is (* + $b(*))(7), 7 + 100, 'one placeholder + call-arg Whatever = 1 arg';

# The wild-cases line from the roast test.
is-deeply (* + 42 + $b(*) + *.flip + *.flip²)(5, 17, 13), 1179, 'wild multi-arg case';

# `*.flip` still curries normally (target contains Whatever).
is (*.flip)('123'), '321', 'method curry on Whatever still works';

# A curried target invoked with a plain arg still curries the target only.
is (*²)(3), 9, 'exponent curry target invoked with plain arg';
