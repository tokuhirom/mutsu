use Test;

# `base-repeating` is declared on `Rational`, so a `FatRat` has it too (#13258),
# on big parts as well; one implementation serves `Rat` and `FatRat`.

plan 10;

is FatRat.new(1, 3).base-repeating, (q{0.}, '3'), 'FatRat 1/3';
is FatRat.new(1, 3).base-repeating(3), ('0.1', ''), 'FatRat 1/3 in base 3';
is FatRat.new(1, 7).base-repeating(2), ('0.', '001'), 'FatRat 1/7 in base 2';
is FatRat.new(-5, 6).base-repeating, ('-0.8', '3'), 'a negative FatRat';
is FatRat.new(2**70 + 1, 3).base-repeating, ('393530540239137101141.', '6'), 'big parts';
is FatRat.new(31, 2).base-repeating(16), ('F.8', ''), 'FatRat in base 16';
is (10/1).base-repeating(16), ('10', ''), 'an integral Rat is its decimal Str';
is FatRat.new(-10, 1).base-repeating(16), ('-10', ''), 'an integral FatRat too';
is (-22/7).base-repeating, ('-3.', '142857'), 'Rat still answers';
throws-like { FatRat.new(1, 3).base-repeating(37).sink }, X::OutOfRange, 'radix out of range';
