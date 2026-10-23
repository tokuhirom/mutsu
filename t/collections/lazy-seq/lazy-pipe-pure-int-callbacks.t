use v6;
use Test;

# A lazy `.grep`/`.map` stage whose callback is `$_ OP constant` over an Int is
# computed by the built-in operator without a closure call (#14370). Results,
# laziness and every shape that must still run the real callback are pinned.

plan 14;

is-deeply (1..*).grep({ $_ %% 3 }).map({ $_ + 1 }).head(5).list, (4, 7, 10, 13, 16), 'grep %% then map +';
is-deeply (1..*).map(* * 2).head(4).list, (2, 4, 6, 8), 'a WhateverCode map';
is-deeply (1..*).map({ $_ - 1 }).head(3).list, (0, 1, 2), 'subtraction';
is-deeply (-5..*).grep({ $_ %% 2 }).head(3).list, (-4, -2, 0), 'negative elements are divisible by sign-blind rules';
is-deeply (1..*).grep(* %% 7).head(2).list, (7, 14), 'a WhateverCode grep';

my @r = (9223372036854775800..*).map({ $_ + 10 }).head(3);
is-deeply @r.Array, [9223372036854775810, 9223372036854775811, 9223372036854775812],
    'a sum past the i64 range promotes to a bigger Int';

my @m = (9223372036854775805..*).map({ $_ * 2 }).head(2);
is-deeply @m.Array, [18446744073709551610, 18446744073709551612], 'a product past the i64 range promotes';

my $calls = 0;
my $pipe = (1..*).map({ $calls++; $_ + 1 });
is-deeply $pipe.head(3).list, (2, 3, 4), 'an impure callback still runs';
is $calls, 3, '... exactly once per pulled element';

is-deeply (1.5, 2.5, 3.5).lazy.map({ $_ + 1 }).eager.list, (2.5, 3.5, 4.5), 'non-Int elements use the real operator';
is-deeply ('1', '2').lazy.map({ $_ + 1 }).eager.list, (2, 3), 'numeric strings use the real operator';

{
    sub infix:<+>(Int $a, Int $b) { 42 }
    is-deeply (1..*).map({ $_ + 1 }).head(2).list, (42, 42), 'a user-declared infix:<+> is honoured';
}

my @taken;
for (1..*).grep({ $_ %% 5 }).map({ $_ * 2 }) { @taken.push($_); last if @taken == 3 }
is-deeply @taken, [10, 20, 30], 'a for loop pulls the chain one element at a time';

is-deeply (1..*).grep({ $_ %% 3 }).map({ $_ + 1 }).head(0).list, (), 'head(0) pulls nothing';
