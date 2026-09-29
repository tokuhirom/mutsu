use Test;

plan 4;

my @o = (1, * + 1 ... *);
is-deeply @o[^2, 20, 30, 40], ((1, 2), 21, 31, 41), 'range + ints in a lazy-array slice';
is-deeply @o[(0, 1), (20, 30)], ((1, 2), (21, 31)), 'nested lists in a lazy-array slice';
is-deeply @o[1..3, 5], ((2, 3, 4), 6), 'range then int';
is-deeply @o[0, 20, 30, 40], (1, 21, 31, 41), 'flat list still works';
