use v6.e;
use Test;

plan 37;

# Numeric, Real
is-deeply 42.Numeric, 42, 'Int.Numeric is the Int';
is-deeply 2.5e0.Numeric, 2.5e0, 'Num.Numeric is the Num';
is-deeply 3.75.Numeric, 3.75, 'Rat.Numeric is the Rat';
is-deeply (3+4i).Numeric, 3+4i, 'Complex.Numeric keeps the imaginary part';
is-deeply (2**70).Numeric, 2**70, 'a big Int is its own Numeric';
is-deeply FatRat.new(1, 2**70).Numeric.denominator, 2**70, 'FatRat.Numeric keeps a big denominator exact';
is-deeply True.Numeric, 1, 'Bool.Numeric is its Int';
is-deeply 42.Real, 42, 'Int.Real';
is-deeply 2.5e0.Real, 2.5e0, 'Num.Real';
is-deeply 3.75.Real, 3.75, 'Rat.Real';
is-deeply False.Real, 0, 'Bool.Real is its Int';

# Rat, FatRat
is-deeply 42.Rat, 42.0, 'Int.Rat';
is-deeply (2**70).Rat, (2**70).Rat, 'a big Int stays a big Rat';
is (2**70).Rat.Str, '1180591620717411303424', 'a big Int is not clamped to a word by .Rat';
is-deeply 0.75e0.Rat, 0.75, 'Num.Rat';
is-deeply NaN.Rat, <0/0>, 'NaN.Rat is 0/0';
is-deeply Inf.Rat, <1/0>, 'Inf.Rat is 1/0';
is-deeply -Inf.Rat, <-1/0>, '-Inf.Rat is -1/0';
is-deeply 3.75.Rat, 3.75, 'Rat.Rat is itself';
is-deeply FatRat.new(1, 3).Rat, 1/3, 'FatRat.Rat drops the precision flag';
is-deeply 3.14159e0.Rat(0.01), 22/7, 'Num.Rat takes an epsilon';
is-deeply 42.Rat(0.01), 42.0, 'Int.Rat ignores the epsilon';
is-deeply 42.FatRat, FatRat.new(42, 1), 'Int.FatRat';
is-deeply 3.75.FatRat, FatRat.new(15, 4), 'Rat.FatRat';
is-deeply 0.75e0.FatRat, FatRat.new(3, 4), 'Num.FatRat';
is-deeply 3.14159e0.FatRat(0.01), FatRat.new(22, 7), 'Num.FatRat takes an epsilon';
is-deeply (2**70).FatRat, FatRat.new(2**70, 1), 'a big Int is a big FatRat';
is-deeply True.Rat, 1.0, 'Bool.Rat is its Int as a Rat';

# Complex
is-deeply 42.Complex, 42+0i, 'Int.Complex';
is-deeply 2.5e0.Complex, 2.5+0i, 'Num.Complex';
is-deeply 3.75.Complex, 3.75+0i, 'Rat.Complex';
is-deeply (3+4i).Complex, 3+4i, 'Complex.Complex is itself';
is-deeply True.Complex, 1+0i, 'Bool.Complex is its Int as a Complex';

# Epsilon type checks stay with the rows
throws-like { 3.75.Rat('0.01') }, X::TypeCheck::Binding::Parameter, 'Rat.Rat(Str) fails the Real bind';
throws-like { 2.5e0.Rat('0.01') }, X::TypeCheck::Binding::Parameter, 'Num.Rat(Str) fails the Real bind';
throws-like { 2.5e0.FatRat('0.01') }, X::TypeCheck::Binding::Parameter, 'Num.FatRat(Str) fails the Real bind';

# type objects keep the cascade's answers
is Int.Numeric.^name, 'Int', 'Int.Numeric on the type object still warns and answers 0';
