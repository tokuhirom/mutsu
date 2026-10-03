use v6;
use Test;

# From SION (zef distribution): its hexfloat decoder builds a Num with
# `FatRat.new($mant, 2 ** -$e).Num`; for 5e-324 the denominator is 2**1074,
# which is not a double, and the old `n.to_f64() / d.to_f64()` gave 0.
plan 8;

is FatRat.new(1, 2 ** 1074).Num, 5e-324, 'FatRat with a 2**1074 denominator';
is FatRat.new(3, 2 ** 1076).Num, 5e-324, 'rounds into the subnormal range';
is FatRat.new(1, 2 ** 1070).Num, 8e-323, 'a larger subnormal';
is FatRat.new(1, 2 ** 1100).Num, 0, 'below the smallest subnormal is 0';
is FatRat.new(2 ** 1100, 2 ** 1090).Num, 1024, 'both parts beyond a double';
is FatRat.new(10 ** 400, 3).Num, Inf, 'too large is Inf';
is Num(FatRat.new(1, 2 ** 1074)), 5e-324, 'Num(...) coercion form';
is Num(FatRat.new(1, 3)), (1/3).Num, 'Num(FatRat) is not 0';
