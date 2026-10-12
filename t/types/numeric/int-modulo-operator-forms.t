use Test;

# `Int % Int` takes a fast path in the VM's modulo opcode (#10111). Every
# answer must be the general path's: floored, with the divisor's sign, exact
# at the i64 edges, a Failure for a zero divisor, and a user `infix:<%>`
# still wins.

plan 10;

my @pairs = (7, 3), (-7, 3), (7, -3), (-7, -3), (0, 5), (6, 3);
is-deeply @pairs.map({ .[0] % .[1] }).List, (1, 2, -2, -1, 0, 0), 'floored modulus, divisor sign';

my ($min, $neg1) = -9223372036854775808, -1;
is $min % $neg1, 0, 'i64 min % -1 is 0';
is 9223372036854775807 % 10, 7, 'i64 max % 10';
is-deeply (2**70 + 5) % 7, (2**70 + 5) mod 7, 'a BigInt dividend';

my $zero = 0;
my $r = 7 % $zero;
isa-ok $r, Failure, 'a zero divisor is a Failure';
throws-like { sink 7 % $zero }, X::Numeric::DivideByZero, 'which throws when sunk';

is-deeply (7/2) % 2, 1.5, 'a Rat operand takes the general path';
is 7e0 % 2, 1e0, 'a Num operand takes the general path';

my $sum = 0;
for ^1000 -> $i { $sum += ($i * 37) % 200 }
is $sum, 99_500, 'in a hot loop';

{
    sub infix:<%>(Int $a, Int $b) { "user" }
    my ($a, $b) = 7, 3;
    is $a % $b, 'user', 'a user infix:<%> takes the call';
}
