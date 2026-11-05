use v6;
use Test;
use nqp;

# A raw BOOTInt/BOOTNum/BOOTStr box is its high-level value at an operator
# (rakudo hllizes at the binder), so arithmetic, comparison, `~`, `x` and the
# prefix operators work on it (#12881).

plan 14;

my $b := nqp::box_s("x", nqp::bootstr());
my $i := nqp::box_i(3, nqp::bootint());
my $n := nqp::box_n(2.5e0, nqp::bootnum());

is ($i + 1).^name, "Int", 'BOOTInt + Int is an Int';
is $i * $i, 9, 'multiplication';
is $i - 1, 2, 'subtraction';
is $i % 2, 1, 'modulo';
is ($n + 1).^name, "Num", 'BOOTNum + Int is a Num';
is $i + $n, 5.5, 'BOOTInt + BOOTNum';
is $b ~ "y", "xy", 'concatenation, left box';
is "y" ~ $b, "yx", 'concatenation, right box';
is $b x 2, "xx", 'repetition';
ok $i == 3, 'numeric equality';
ok $b eq "x", 'string equality';
is $b cmp "a", More, 'cmp';
is -$i, -3, 'prefix minus';
is +$i, 3, 'prefix plus';
