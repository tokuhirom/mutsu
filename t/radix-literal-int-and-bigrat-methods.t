use Test;

# Two independent gaps found together in roast/S02-literals/radix.t.
#
# 1. A generic radix *literal* whose fractional digits are all zero is an Int,
#    even though the same digits as a plain literal would be a Rat. Rakudo's
#    `radcalc` decides on whether the fraction evaluates to zero, not on
#    whether the final value happens to be integral — which is why
#    `:16<f.8*16**2>` stays Rat despite being 3968.
# 2. A rational whose numerator outgrew i64 is a BigRat, and the numeric
#    methods only knew Rat, so `.abs` / `.sign` / `.sqrt` reported
#    "No such method ... for invocant of type 'Rat'" and `.Int` silently
#    clamped to i64::MAX.

plan 16;

# --- 1. radix literal types ---------------------------------------------

is :10<2.0>.WHAT.^name,   'Int', ':10<2.0> is an Int';
is :10<2.0>,              2,     ':10<2.0> is 2';
is :2<1.0>.WHAT.^name,    'Int', ':2<1.0> is an Int';
is :16<f*16**1>.WHAT.^name, 'Int', 'a radix literal with an exponent and no fraction is an Int';
is :16<f*16**1>,          240,   ':16<f*16**1> is 240';
is :16<dead_beef*16**8>.WHAT.^name, 'Int', 'an exponent past i64 range is still an Int';
is :16<dead_beef*16**8>,  16045690981097406464, 'and carries the exact value';

# A non-zero fraction stays a Rat even when the exponent makes it integral.
is :16<f.8*16**2>.WHAT.^name, 'Rat', 'a non-zero fraction stays a Rat';
is :16<f.8*16**2>,        3968,  'and still has the right value';
is :10<2.5>.WHAT.^name,   'Rat', ':10<2.5> is a Rat';

# --- 2. numeric methods on a big rational --------------------------------

my $big = 16045690981097406464/1;
is $big.abs,  16045690981097406464, '.abs on a big rational';
is (-$big).abs, 16045690981097406464, '.abs of a negative big rational';
is $big.sign, 1,  '.sign on a big rational';
is (-$big).sign, -1, '.sign of a negative big rational';
is $big.Int,  16045690981097406464, '.Int does not clamp to i64::MAX';
is-approx $big.sqrt, 4005707300.976621, '.sqrt on a big rational';
