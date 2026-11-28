use Test;

# `.map` over an Array runs the rw map loop, which aliases `$_` to each
# element so a block can write through it. A body that cannot observe its
# topic's container takes the plain topic instead. These pin both sides.

plan 14;

my @a = 1..5;
is-deeply @a.map(* * 2).List, (2, 4, 6, 8, 10), 'a WhateverCode over an Array';
is-deeply @a.map({ $_ + 1 }).List, (2, 3, 4, 5, 6), 'an implicit-topic block';
is-deeply @a, [1, 2, 3, 4, 5], 'the source is untouched';

my @b = 1..3;
@b.map({ $_ *= 10 }).eager;
is-deeply @b, [10, 20, 30], 'a block that writes $_ still writes through';
my @c = 1..3;
@c.map(* += 1).eager;
is-deeply @c, [2, 3, 4], 'a WhateverCode that writes still writes through';
my @d = 1..3;
@d.map(-> $x is rw { $x = 0 }).eager;
is-deeply @d, [0, 0, 0], 'an is rw parameter still writes through';

my @e = 'a', 'b';
is-deeply @e.map(*.uc).List, <A B>, 'a method call on the topic';
is-deeply @e.map({ .chars }).List, (1, 1), 'a bare method call';

my @nums = 1..6;
is-deeply @nums.map({ next if $_ %% 2; $_ }).List, (1, 3, 5), 'next in the body';
is-deeply @nums.map({ last if $_ > 2; $_ }).List, (1, 2), 'last in the body';

$_ = 'outer';
is-deeply @a.map({ $_ }).List, (1, 2, 3, 4, 5), 'the topic is the element';
is $_, 'outer', 'the outer topic is restored';

my @with-nil = 1, Nil, 3;
is @with-nil.map({ $_.defined }).List, (True, False, True), 'a Nil element';

my @nested = [1, 2], [3, 4];
is-deeply @nested.map(*.elems).List, (2, 2), 'Array elements';
