use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): `so`
# and `not` are prefix operators, so they stack under a tighter prefix and the
# whole chain takes the LOOSEST prefix's precedence. Reading the operand of `!`
# as an ordinary tight prefix expression left `so` as a bare term, after which
# the `*` of `!so *` was an infix multiply with nothing on its right — a hard
# parse error on `sub f($a where !so *)` (Linux::NFTables).

plan 9;

# The construct the index reduced to: a `where` clause holding a prefixed
# whatever-curry.
sub accepts-false(Bool $a where !so *) { 'ok' }
is accepts-false(False), 'ok', 'a where clause takes a prefixed whatever-curry';
dies-ok { accepts-false(True) }, 'and the constraint still rejects True';

# The curry itself: the whole prefix chain currys, giving a WhateverCode.
my $negated = !so *;
isa-ok $negated, Callable, '!so * is a WhateverCode';
is $negated(False), True, '!so * negates its argument';
is $negated(1), False, '!so * is false for a true argument';
is (?so *)(1), True, '?so * curries too';

# The loosest prefix in the chain still owns the whole expression, so an infix
# to the right of a non-Whatever operand is swallowed by `so`, not left to `!`.
is (!so 1 == 2), True, '!so 1 == 2 is !(so(1 == 2))';
is (!so 2 == 2), False, '!so 2 == 2 is !(so(2 == 2))';
is (!not 0), False, '!not 0 stacks the same way';
