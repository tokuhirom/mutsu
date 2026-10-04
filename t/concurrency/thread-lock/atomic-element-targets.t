use Test;
use nqp;

# Atomic fetch / store / RMW on an array or hash element (#11812): the atomic
# routines, the `⚛` operators and the `_i` nqp ops all reach the element's
# atomic cell, the one `cas(@a[0], ...)` swaps. Expected answers are Rakudo
# 2026.09's.

plan 17;

# Declared before any thread starts: an aggregate declared after the first
# `start` hits #11833.
my int @a = 1, 2;
my atomicint @b = 0, 0;
my atomicint @c = 0;
my atomicint @d = 0, 0, 0;

is nqp::atomicinc_i(@a[0]), 1, 'nqp::atomicinc_i on an element answers the old value';
is-deeply @a.List, (2, 2), '... and increments the element';
is nqp::atomicload_i(@a[1]), 2, 'nqp::atomicload_i on an element';
is nqp::atomicadd_i(@a[1], 10), 2, 'nqp::atomicadd_i on an element';
is nqp::atomicstore_i(@a[0], 7), 7, 'nqp::atomicstore_i on an element';
is nqp::cas_i(@a[0], 7, 8), 7, 'nqp::cas_i on an element';
is-deeply @a.List, (8, 12), 'all of them wrote the element';

is atomic-fetch-inc(@b[1]), 0, 'atomic-fetch-inc on an element';
@b[0]⚛++;
++⚛@b[0];
is ⚛@b[0], 2, 'postfix ⚛++, prefix ++⚛ and prefix ⚛ on an element';
@b[1]⚛--;
is --⚛@b[1], -1, 'postfix ⚛-- and prefix --⚛ on an element';
is atomic-add-fetch(@b[0], 5), 7, 'atomic-add-fetch on an element';
is atomic-fetch-sub(@b[0], 2), 7, 'atomic-fetch-sub on an element';
is-deeply @b.List, (5, -1), '... and the element holds the result';
atomic-assign(@b[1], 9);
is atomic-fetch(@b[1]), 9, 'atomic-assign / atomic-fetch on an element';

my $i = 1;
is atomic-inc-fetch(@d[$i]), 1, 'a computed index';
is-deeply @d.List, (0, 1, 0), '... updates that element only';

await (^4).map: { start { @c[0]⚛++ for ^250 } };
is @c[0], 1000, 'element ⚛++ from four threads loses no increment';
