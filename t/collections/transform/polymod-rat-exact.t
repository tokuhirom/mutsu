use v6;
use Test;

plan 9;

# Rat divisors must be decomposed in exact rational arithmetic, not float —
# otherwise the results accrete noise like (0.20000000000000018 ...).
is 5.Rat.polymod(.3, .2).gist, '(0.2 0 80)', 'Rat divisors stay exact';
is (2/3).polymod(1/3).gist,    '(0 2)',      'fractional invocant and divisor';
is (⅔).polymod(⅓).gist,        '(0 2)',      'unicode fraction literals';

# The results are genuine Rats / Ints, not Nums with float tails
is 5.Rat.polymod(.3, .2)[0], 0.2, 'first remainder equals 1/5 exactly';
ok 5.Rat.polymod(.3, .2)[0] == 1/5, 'remainder compares equal to 1/5';
is 5.Rat.polymod(.3, .2)[2].WHAT.gist, '(Int)', 'final quotient is an Int';

# Integer polymod is unchanged (still exact, matches the docs)
is 120.polymod(10).gist,    '(0 12)',  '120.polymod(10)';
is 120.polymod(10,10).gist, '(0 2 1)', '120.polymod(10,10)';

# Rat invocant with mixed Rat/Int divisors
is (7/2).polymod(1/2, 2).gist, '(0 1 3)', 'Rat invocant, mixed Rat/Int divisors';
