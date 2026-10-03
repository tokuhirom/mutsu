use Test;

# The `Rational` role's methods (`numerator`, `denominator`, `nude`, `norm`,
# `isNaN`) and the other numeric `isNaN`s are rows in the built-in method
# table (ADR-11276). The same answer must come back whether the call is on a
# variable (the call-site lane), an inline value, or through the interpreter,
# and for machine-word and big components alike.

plan 30;

my $r = 6/4;
is $r.numerator,   3, 'Rat.numerator on a variable';
is $r.denominator, 2, 'Rat.denominator on a variable';
is-deeply $r.nude.List, (3, 2), 'Rat.nude';
is (6/4).numerator, 3, 'Rat.numerator on an inline value';
is <0/0>.isNaN, True, 'Rat 0/0 is NaN';
is (1/3).isNaN, False, 'an ordinary Rat is not NaN';
is (1/3).norm.WHAT, Rat, 'Rat.norm is a Rat';

my $f = FatRat.new(6, 4);
is $f.numerator,   3, 'FatRat.numerator';
is $f.denominator, 2, 'FatRat.denominator';
is-deeply $f.nude.List, (3, 2), 'FatRat.nude';
is $f.norm.WHAT, FatRat, 'FatRat.norm is a FatRat';
is FatRat.new(0, 0).isNaN, True, 'FatRat 0/0 is NaN';
is FatRat.new(0, 0).norm.WHAT, FatRat, 'a NaN FatRat normalizes to a FatRat';

my $big = 2**70 / 3;
is $big.WHAT, Rat, 'a Rat with a big numerator';
is $big.numerator, 1180591620717411303424, 'big Rat.numerator';
is $big.denominator, 3, 'big Rat.denominator';
is $big.norm.WHAT, Rat, 'big Rat.norm is a Rat';

my $bigf = FatRat.new(2**70, 3);
is $bigf.numerator, 1180591620717411303424, 'big FatRat.numerator';
is $bigf.norm.WHAT, FatRat, 'big FatRat.norm is a FatRat';

is 5.isNaN, False, 'Int.isNaN';
is (2**70).isNaN, False, 'big Int.isNaN';
is Int.isNaN, False, 'Int.isNaN on the type object';
is NaN.isNaN, True, 'Num NaN';
is 1e0.isNaN, False, 'an ordinary Num';
is (NaN+1i).isNaN, True, 'Complex with a NaN part';
is (1+2i).isNaN, False, 'an ordinary Complex';
is True.isNaN, False, 'Bool.isNaN';

# Rakudo's Int does not do Rational.
throws-like { 5.numerator }, X::Method::NotFound, 'Int has no numerator';
throws-like { (2**70).nude }, X::Method::NotFound, 'big Int has no nude';
throws-like { 1e0.denominator }, X::Method::NotFound, 'Num has no denominator';
