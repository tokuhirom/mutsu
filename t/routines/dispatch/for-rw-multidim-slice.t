use Test;

# #19339: an rw `for` over a multi-dimensional slice aliases the leaves.
plan 8;

my @b[2;3] = (1, 2, 3), (4, 5, 6);
for @b[1;*] <-> $x { $x *= 10 }
is-deeply @b[1].List, (40, 50, 60), 'a shaped array row slice writes back';
is-deeply @b[0].List, (1, 2, 3), 'the other row is untouched';

for @b[*;1] <-> $x { $x += 1 }
is-deeply (@b[0;1], @b[1;1]), (3, 51), 'a column slice writes back';

my @c = [1, 2], [3, 4];
for @c[1;*] <-> $x { $x *= 10 }
is-deeply @c[1].List, (30, 40), 'a nested plain array slice writes back';

for @c[0;0, 1] <-> $x { $x += 100 }
is-deeply @c[0].List, (101, 102), 'an index list slice writes back';

for @c[1;*] { $_ += 1 }
is-deeply @c[1].List, (31, 41), 'the topic aliases the leaves too';

my @seen;
for @c[*;0] -> $x { @seen.push($x) }
is-deeply @seen, [101, 31], 'a read-only loop sees the leaves';
is-deeply @c[0;0], 101, 'and leaves them alone';
