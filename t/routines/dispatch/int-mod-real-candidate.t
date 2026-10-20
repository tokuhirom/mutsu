use Test;

# An Int left side with a non-Int Real right side takes Rakudo's `Real, Real`
# candidate, `$a - ($a div $b) * $b`, whose `div` coerces $b to Int (#14119).

plan 17;

is 10 mod 2.5, -2.5, '10 mod 2.5';
is 7 mod 2.5, -0.5, '7 mod 2.5';
is-deeply 7 mod 2.5e0, -0.5e0, '7 mod 2.5e0 is a Num';
is -7 mod 2.5, 3, '-7 mod 2.5';
is 7 mod -2.5, -3, '7 mod -2.5';
is 7 mod 1.5, -3.5, '7 mod 1.5';
is 7 mod 3.0, 1, '7 mod 3.0';
is-deeply 7 mod 2.5.FatRat, (-0.5).FatRat, '7 mod 2.5.FatRat stays a FatRat';
is (2**70) mod 2.5, -295147905179352825856, 'a BigInt left side';
isa-ok (7 mod 2.5), Rat, 'an Int mod a Rat is a Rat';
is-deeply ([mod] (7, 2.5)), -0.5, 'the [mod] reduction agrees';
is 7.5 mod 2, 1.5, 'a Rat left side is unchanged';
is 7 mod 2, 1, 'Int mod Int is unchanged';
is 10 % 2.5, 0, '% keeps the floored remainder: 10 % 2.5';
is 7 % 2.5, 2, '7 % 2.5';
is -7 % 2.5, 0.5, '-7 % 2.5';
dies-ok { my $x = 7 mod 0.5; $x.Str }, '7 mod 0.5 divides by zero (0.5.Int is 0)';

done-testing;
