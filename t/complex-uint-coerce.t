use Test;

# Complex.UInt coerces via the real part (a purely-real Complex), truncating
# like .Int; a non-zero imaginary part is not Real and throws X::Numeric::Real.
# Regression: `(5+0i).UInt` was "No such method 'UInt' for Complex".

plan 6;

is (5 + 0i).UInt,   5, '(5+0i).UInt -> 5';
is (5.7 + 0i).UInt, 5, '(5.7+0i).UInt truncates -> 5';
is (0 + 0i).UInt,   0, '(0+0i).UInt -> 0';

# non-zero imaginary part is not Real
throws-like { (5 + 2i).UInt }, X::Numeric::Real,
    'UInt of a Complex with imaginary part throws X::Numeric::Real';

# a purely-real Complex still coerces even when written with -0i
is (42 - 0i).UInt, 42, '(42-0i).UInt -> 42';

# .UInt matches .Int on the real value for a real Complex
is (9 + 0i).UInt, (9 + 0i).Int, 'Complex.UInt matches .Int for a real Complex';
