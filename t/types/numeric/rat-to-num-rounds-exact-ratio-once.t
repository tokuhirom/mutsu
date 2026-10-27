use Test;

# From Data::Transformers (t/05-standardize.rakutest): a Rat whose numerator is
# past 2**53 converts to the double nearest the exact ratio. The conversion
# truncated its quotient and then rounded it, so a ratio just past a rounding
# midpoint came out one ulp low and `standardize`'s results differed in the
# last digit.
plan 5;

is Rat.new(-29849362802126903, 6250000000000000).Num.raku, '-4.775898048340305e0',
    'a ratio past a rounding midpoint rounds up';
is Rat.new(-9992370000000001, 250000000000000).Num.raku, '-39.969480000000004e0',
    'a ratio the literal spells exactly';
is Rat.new(1, 3).Num.raku, '0.3333333333333333e0', '1/3';
is Rat.new(123456789012345678901234567, 1000000000000000000000000).Num.raku,
    '123.45678901234568e0', 'a ratio of big integers';
my $d = 0.8637835989968501 - 5.63968164733715458;
is ($d / 3.8411832001595734e0).raku, '-1.243340345792906e0', 'Rat / Num goes through the exact Rat.Num';
