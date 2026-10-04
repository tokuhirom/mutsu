use Test;

plan 18;

# Both inline and variable receivers must reach the same method body. These
# cases also pin promotion, rational precision, Complex stepping and Str's
# non-numeric rules while succ/pred move from the cascade into rows.
is 5.succ, 6, 'inline Int.succ';
is 5.pred, 4, 'inline Int.pred';
my $int = 5;
is $int.succ, 6, 'variable Int.succ';
is $int.pred, 4, 'variable Int.pred';
is 9223372036854775807.succ, 9223372036854775808, 'Int.succ promotes';
is (-9223372036854775808).pred, -9223372036854775809, 'Int.pred promotes';

my $num = 1.5e0;
is $num.succ, 2.5e0, 'Num.succ';
is $num.pred, 0.5e0, 'Num.pred';
my $rat = 1/3;
is $rat.succ, 4/3, 'Rat.succ stays exact';
is $rat.pred, -2/3, 'Rat.pred stays exact';
my $fat = FatRat.new(1, 3);
is $fat.succ, FatRat.new(4, 3), 'FatRat.succ';
is $fat.pred, FatRat.new(-2, 3), 'FatRat.pred';

my $complex = 2+3i;
is $complex.succ, 3+3i, 'Complex.succ';
is $complex.pred, 1+3i, 'Complex.pred';
my $str = 'az';
is $str.succ, 'ba', 'Str.succ carries';
is $str.pred, 'ay', 'Str.pred';
isa-ok 'a'.pred, Failure, 'Str.pred underflow returns a Failure';

class Custom { method succ { 'custom' } }
is Custom.new.succ, 'custom', 'a user method still wins';
