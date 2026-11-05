use v6;
use Test;

# `.base` of a Rat/FatRat whose numerator or denominator no longer fits a
# machine word (ClickHouse's `rational-to-decimal` renders a twenty-digit
# FatRat through `$r.base(10, $digits)`).

plan 4;

my $f = '0.33333333333333333333'.FatRat;
is $f.base(10, 20), '0.33333333333333333333', 'twenty exact digits';
is $f.base(10, 5), '0.33333', 'fewer digits round';
is '0.5000000000000000000000'.FatRat.base(10, 3), '0.500', 'trailing zeros are kept';
is '-12.250000000000000000000'.FatRat.base(10, 2), '-12.25', 'sign and integer part';
