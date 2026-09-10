use v6;
use Test;

# ADR-0044 D1 section 2.3: the seven core listops must be genuine callable
# `Sub` values, not just a compile-time syntactic rewrite. Before this,
# `&splice(...)` died with "Unknown function: splice", and `&push(...)` /
# a routine value captured from `&push` silently did nothing -- the array
# was never mutated even though the call itself "succeeded". Assert the
# actual mutation here (not just the return value), since a return-value-only
# check would have passed vacuously on the silent no-op bug.

plan 6;

my @a = (1, 2, 3, 4, 5);
is-deeply &splice(@a, 1, 2), [2, 3],
    '&splice(@a, 1, 2) returns the removed slice';
is-deeply @a, [1, 4, 5], 'and it actually mutated @a (not a silent no-op)';

my @b = (1, 2, 3);
&push(@b, 7);
is-deeply @b, [1, 2, 3, 7], '&push(@b, 7) actually mutates @b';

my @c = (1, 2, 3);
my &f = &push;
f(@c, 7);
is-deeply @c, [1, 2, 3, 7],
    'a routine value captured via my &f = &push; f(...) still mutates';

my @d = (1, 2, 3);
is &pop(@d), 3, '&pop(@d) returns the popped element';
is-deeply @d, [1, 2], 'and it actually mutated @d';
