use Test;

# Rakudo declares `base` on `Rational`, so `FatRat` has the `base` rows too
# (ADR-11276 §9.104): the native cascade's 1-arg `base` arm is gone.

plan 4;

is FatRat.new(1, 3).base(3), '0.1', 'FatRat.base($radix)';
is FatRat.new(7, 2).base(16), '3.8', 'a hexadecimal fraction';
is FatRat.new(1, 3).base(3, 2), '0.10', 'FatRat.base($radix, $digits)';
throws-like { FatRat.new(1, 3).base(37) }, X::OutOfRange, 'a radix out of range';
