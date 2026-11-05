use v6;
use Test;

# From Rat::Precise: `round($rat)` (the routine form) rounded through an f64, so a
# Rat past 2**53 lost its low digits, and `.narrow` left a big-numerator rational
# with a unit quotient a Rat.

plan 12;

my $r = (.5**23) * 100000000000000000000000;
is $r.round, 11920928955078125, '.round of a big-valued Rat is exact';
is round($r), 11920928955078125, 'round() of the same Rat is exact too';
is round(11920928955078125.0), 11920928955078125, 'round() of a Rat literal past 2**53';
is round(-11920928955078125.0), -11920928955078125, 'and a negative one';
is round(20000000000000000001/2), 10000000000000000001, 'a half rounds up';
is round(2.5), 3, 'a small Rat still rounds half up';
is round(-2.5), -2, 'and half toward +Inf for a negative one';

isa-ok (5**33 / 5).narrow, Int, 'a big-numerator rational with a unit quotient narrows to Int';
is (5**33 / 5).narrow, 5**32, 'and keeps the value';
isa-ok (2**70 / 2).narrow, Int, 'also for a power of two';
isa-ok (2**70 / 3).narrow, Rat, 'a non-integral big rational stays a Rat';
isa-ok FatRat.new(2**70, 1).narrow, Int, 'a FatRat with a big unit quotient narrows too';
