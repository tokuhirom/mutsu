use Test;

plan 4;

is-deeply <1/0>, 1/0, '<1/0> parses as zero-denominator Rat';
is-deeply <3/2>, 3/2, '<3/2> parses as Rat';
is-deeply <42>, 42, '<42> parses as Int';
throws-like { 10 / 0e0 }, X::Numeric::DivideByZero, '/ with Num zero throws X::Numeric::DivideByZero';
