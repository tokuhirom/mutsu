# The `+`/`-`/`*`/`/` opcodes answer a plain Rat/Int pair whose result fits an
# i64 Rat on a fast path (#19312). It must give exactly what the general path
# gives: the same reduced value and type, and it must leave to the general path
# a zero divisor, a result that does not fit (which degrades by
# `$*RAT-OVERFLOW`), a FatRat, and a user operator.
use Test;

plan 17;

is-deeply 1/3 + 1/6, 1/2, 'Rat + Rat reduces';
is-deeply (1/2 + 1/2).WHAT, Rat, 'a whole-number sum stays a Rat';
is-deeply 3/4 - 1, -1/4, 'Rat - Int';
is-deeply 2 * 3/4, 3/2, 'Int * Rat';
is-deeply (2/3) / (4/9), 3/2, 'Rat / Rat';
is-deeply (my $a = 7) / (my $b = 2), 7/2, 'Int / Int is a Rat';
is-deeply ((my $c = 8) / (my $d = 4)).WHAT, Rat, 'an even Int / Int is still a Rat';
is-deeply (my $n = -6) / (my $m = 4), -3/2, 'a negative quotient reduces with the sign on top';
is ((my $z = 1) / (my $zz = 0)).^name, 'Rat', 'Int / 0 is a (lazy) Rat, as before';
throws-like { my $q = (1/2) / 0; $q.Str }, X::Numeric::DivideByZero, 'Rat / 0 still fails';

my $big = 9223372036854775807 / 2;
is ($big + 1/3).^name, 'Rat', 'a numerator past i64 keeps the general path and stays a Rat';
my $huge = 9223372036854775807;
my $three = 3;
is ((1/$huge) * (1/$three)).^name, 'Num', 'a denominator past uint64 degrades to Num';
{
    my $*RAT-OVERFLOW = FatRat;
    is ((1/$huge) * (1/$three)).^name, 'FatRat', '$*RAT-OVERFLOW still decides an overflow';
}
is-deeply FatRat.new(1, 3) + 1/3, FatRat.new(2, 3), 'a FatRat operand keeps its type';

my $t = 0;
$t = $t + 1/3 for ^3000;
is-deeply $t, 1000/1, 'a loop of Rat adds';

{
    multi sub infix:<+>(Rat $a, Rat $b) { 'mine' }
    is 1/3 + 1/6, 'mine', 'a user infix:<+> candidate still wins';
}
is 1/3 + 1/6, 1/2, 'outside its scope the core operator answers';
