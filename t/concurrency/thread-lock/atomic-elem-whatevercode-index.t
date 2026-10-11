use Test;
plan 5;

my @neg = 1, 2, 3;
cas(@neg[*-1], 3, 30);
is-deeply @neg, [1, 2, 30], 'cas with a *-1 index swaps the last element';

my @c = 1, 2, 3;
cas(@c[*-2], -> $v { $v + 10 });
is-deeply @c, [1, 12, 3], 'code-form cas with *-2 works on the second to last';

my atomicint @n = 5, 6, 7;
atomic-fetch-inc(@n[*-1]);
is-deeply @n.List, (5, 6, 8), , 'atomic-fetch-inc with *-1';
is atomic-fetch(@n[*-1]), 8, 'atomic-fetch with *-1';
is atomic-fetch-dec(@n[*-3]), 5, 'atomic-fetch-dec with *-3 returns old value';
