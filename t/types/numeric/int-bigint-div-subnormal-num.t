use v6;
use Test;

# `Int / BigInt` degrading to a Num (the exact big-rational denominator
# exceeds uint64, so `make_big_rat_arith` falls back to a float quotient)
# used to underflow to 0 for a sufficiently large BigInt denominator,
# instead of the correct tiny/subnormal double. Root cause: `bigrat_to_f64`
# computed the result as `qf * 2.0f64.powi(-shift)` in two separate
# floating-point steps, and the standalone factor `2.0f64.powi(-shift)`
# itself underflows to exactly 0.0 once `shift` exceeds the smallest
# subnormal double's exponent magnitude (~1074) -- even though the
# correctly-rounded PRODUCT with `qf` is still representable (#8530).

plan 6;

is 1 / 2**1023, 1.1125369292536007e-308,
    'Int / BigInt degrading to Num produces the correct subnormal quotient';
is 1e0 / 2**1023, 1.1125369292536007e-308,
    'Num / BigInt already worked and stays correct';
is (1 / 2**1023).WHAT, Num,
    'the extreme-denominator division coerces straight to Num, not Rat';
is (1 / 2**1023).Num, 1.1125369292536007e-308,
    '.Num on the already-Num result is unaffected';
is FatRat.new(1, 2**1023).Num, 1.1125369292536007e-308,
    'FatRat.new(...).Num already worked (a different, unaffected code path) and stays correct';
is (1 / 2**1024).Num, 5.562684646268003e-309,
    'an even more extreme denominator (needing a larger shift) is still correct';
