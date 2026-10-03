use Test;

# A `for` loop over an array slice aliases each selected element: an rw
# loop variable (or the topic) writes back into the array.

plan 9;

my @a = 1, 2, 3;
for @a[1, 2] <-> $t { $t *= 10 }
is-deeply @a, [1, 20, 30], '<-> over a list slice';

my @b = 1, 2, 3;
for @b[1, 2] { $_ *= 10 }
is-deeply @b, [1, 20, 30], 'topic over a list slice';

my @c = 1, 2, 3;
for @c[1, 2] -> $t is rw { $t *= 10 }
is-deeply @c, [1, 20, 30], 'is rw over a list slice';

my @d = 1, 2, 3, 4;
for @d[1..2] { $_ = 0 }
is-deeply @d, [1, 0, 0, 4], 'range slice';

my @e = 'a', "b\n# c", "d\n# e";
for @e[1..*-1] <-> $t { $t = $t.subst(/^^ '#' \s*/, '') }
is-deeply @e, ['a', "b\nc", "d\ne"], 'range with a WhateverCode end';

my @f = 1, 2, 3, 4;
for @f[1..*] <-> $t { $t = 0 }
is-deeply @f, [1, 0, 0, 0], 'range to Inf';

my @g = 1, 2, 3;
my @seen;
for @g[1, 2] { @seen.push($_) }
@g[1] = 99;
is-deeply @seen, [2, 3], 'values taken from the topic are copies';

my @h;
for @h[0, 1] { }
is @h.elems, 0, 'a slice past the end does not vivify';

my @s := @g[1..*-1];
@s[0] = 5;
is @g[1], 5, 'a bound range slice with a WhateverCode end aliases';
