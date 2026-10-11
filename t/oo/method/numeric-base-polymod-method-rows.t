use v6.e;
use Test;

plan 36;

# base: Int
is 255.base(16), 'FF', 'Int.base(16)';
is 255.base(2), '11111111', 'Int.base(2)';
is (-255).base(16), '-FF', 'a negative Int';
is 0.base(2), '0', 'zero';
is 255.base("16"), 'FF', 'the radix may be an integer-valued Str';
is (2**70).base(36), (2**70).base(36).uc, 'a big Int in base 36 is upper case';
is (2**70).base(16), '400000000000000000', 'a big Int in base 16';
is 255.base(16, 3), 'FF.000', 'Int.base with a digits count pads the fraction';
is (2**70).base(16, 3), '400000000000000000.000', 'a big Int with a digits count';
is True.base(2), '1', 'Bool.base is its Int';

# base: Num and Rat
is 0.5e0.base(2), '0.1', 'Num.base(2)';
is 3.75.base(2), '11.11', 'Rat.base(2)';
is 3.75.base(16, 1), '3.C', 'Rat.base with a digits count';
is (1/3).base(3), '0.1', '1/3 in base 3';
is FatRat.new(1, 3).base(3), '0.1', 'FatRat.base';
is (1/4).base(10, *), '0.25', 'a Whatever digits count';
throws-like { NaN.base(2) }, X::Numeric::CannotConvert, 'NaN.base fails';
throws-like { Inf.base(16, 3) }, X::Numeric::CannotConvert, 'Inf.base with digits fails too';

# base: bad radix
my $bad = 255.base(37);
isa-ok $bad, Failure, 'a radix over 36 is a Failure';
is $bad.exception.^name, 'X::OutOfRange', '... an X::OutOfRange';
like $bad.exception.message, /'Is: 37, should be in 2..36'/, '... that names the radix and the range';
$bad.so;
is 255.base(1).exception.^name, 'X::OutOfRange', 'a radix under 2';

# base-repeating
is-deeply (1/3).base-repeating, ('0.', '3'), '1/3 repeats 3 in base 10';
is-deeply (1/3).base-repeating(3), ('0.1', ''), '1/3 terminates in base 3';
is-deeply (1/6).base-repeating, ('0.1', '6'), '1/6 has a non-repeating digit';

# polymod
is-deeply 120.polymod(60), (0, 2).Seq, 'Int.polymod with one divisor';
is-deeply 3725.polymod(60, 60), (5, 2, 1).Seq, 'Int.polymod splits seconds into h:m:s';
is-deeply 0.polymod(3), (0, 0).Seq, 'zero';
is-deeply 5.polymod(), (5,).Seq, 'no divisors leaves the number';
is-deeply 10.polymod(3, 3, 3), (1, 0, 1, 0).Seq, 'every divisor consumes a digit';
is 5.Rat.polymod(.3, .2).join(','), '0.2,0,80', 'Rat.polymod stays exact';
is-deeply 125.polymod(10 xx *), (5, 2, 1).Seq, 'a lazy divisor list stops with the number';
is-deeply Duration.new(3725).polymod(60, 60).map(*.Num), (5e0, 2e0, 1e0).Seq, 'Duration.polymod';
is-deeply 600.polymod(gather { take 10; take 10 }), (0, 0, 6).Seq, 'a gather list of divisors';
throws-like { "10".polymod(3) }, X::Method::NotFound, 'Str has no polymod';
throws-like { (1+2i).polymod(3) }, X::Method::NotFound, 'Complex has no polymod';
