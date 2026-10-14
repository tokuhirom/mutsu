use Test;

plan 7;

# A single-star compound assignment binds its argument's container, so a
# direct call writes back (S02 "WhateverCode parameters are rw").
my $x = 1;
(* += 1)($x);
is $x, 2, '(* += 1)($x) writes back';

my $y = 3;
(* *= 2)($y);
is $y, 6, '(* *= 2)($y) writes back';

my $z = 3;
(* += *)($z, 5);
is $z, 8, '(* += *)($z, 5) writes back';

# The map paths keep writing through to the source elements.
my @a = 1, 2, 3;
@a.map(* += 1).eager;
is-deeply @a, [2, 3, 4], 'map(* += 1) on a plain array';

my @b[3] = 1, 2, 3;
@b.map(* += 1).eager;
is-deeply @b, [2, 3, 4], 'map(* += 1) on a shaped array';

my int @c[3] = 1, 2, 3;
@c.map(* *= 2);
is @c.join(","), "2,4,6", 'map(* *= 2) on a native shaped array';

# A read-only WhateverCode still does not touch its argument.
my $r = 5;
is (* + 1)($r), 6, 'read-only WhateverCode leaves its argument alone';
