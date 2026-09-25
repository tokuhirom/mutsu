use Test;

# FatRat arithmetic must stay FatRat once its numerator/denominator outgrow
# 64 bits, and `.round($scale)` takes the type of `Int * $scale`. Reduced from
# the BigRoot distribution's Newton iteration, whose `my FatRat $diff =
# abs($new - $guess)` died with "expected FatRat but got Rat".

plan 12;

my $a = FatRat.new(10**20 + 1, 10**20 * 3);
my $b = FatRat.new(10**20 + 7, 10**20 * 7);

isa-ok abs($a - $b), FatRat, 'abs of a big FatRat';
isa-ok ($a - $b).abs, FatRat, '.abs of a big FatRat';
isa-ok abs(-$a), FatRat, 'abs of a negated big FatRat';
isa-ok $a ** 2, FatRat, 'big FatRat ** positive Int';
isa-ok $a ** -2, FatRat, 'big FatRat ** negative Int';

isa-ok FatRat.new(3, 2).round(FatRat.new(1, 10)), FatRat, 'FatRat.round(FatRat)';
isa-ok (3/2).round(FatRat.new(1, 10)), FatRat, 'Rat.round(FatRat)';
isa-ok 15.round(FatRat.new(1, 10)), FatRat, 'Int.round(FatRat)';
isa-ok FatRat.new(3, 2).round(1/10), Rat, 'FatRat.round(Rat) is a Rat';
is FatRat.new(2, 3).round(FatRat.new(1, 10**40)),
    '0.6666666666666666666666666666666666666667',
    'rounding to a 40-digit FatRat scale keeps every digit';
is round(FatRat.new(2, 3), FatRat.new(1, 10**40)),
    '0.6666666666666666666666666666666666666667',
    'the sub form agrees';
is (2/3).round(1/10**30), 0.6666666666666667, 'a Rat scale past uint64 still degrades';
