use Test;

plan 4;

# `flat` (and list flattening) must splice a native shaped array's elements in,
# like any other list — previously the native array stayed a single opaque item.

my @native := array[int].new(:shape(10), 1..10);

is-deeply (flat 0, @native, 11).elems, 12, 'flat splices a native shaped array';
is-deeply (flat 0, @native, 11)[5], 5,     'flat-spliced native array values are accessible';

my @untyped = flat 0, @native, 11;
is-deeply @untyped.elems, 12, 'list-assign of flat with a native array flattens';

# A plain @-array still flattens too (regression guard).
my @a = 1, 2, 3;
is-deeply (flat 0, @a, 4), (0, 1, 2, 3, 4), 'plain array still flattens under flat';
