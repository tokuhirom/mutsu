use v6;
use Test;

my @a = 1, 2, 3;
for @a.tail(2) { $_ = 0 }
is-deeply @a, [1, 0, 0], '.tail(n) yields the original Array elements';
for @a.head(2) { $_ = 5 }
is-deeply @a, [5, 5, 0], '.head(n) yields the original Array elements';

my @h;
@h[2] = 3;
for @h.head(2) { $_ = 9 }
is-deeply @h, [9, 9, 3], '.head(n) can write through holes';

my @list := (1, 2, 3);
try { for @list.head(2) { $_ = 0 } }
is-deeply @list, (1, 2, 3), 'immutable List remains unchanged';

done-testing;
