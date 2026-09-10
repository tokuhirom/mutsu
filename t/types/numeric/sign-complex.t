use Test;

plan 7;

# `sign` requires a Real. A Complex with a non-zero imaginary part cannot be
# coerced to Real, so it throws X::Numeric::Real; a purely real Complex yields
# the Int sign of its real part.

throws-like { sign(3+4i) }, X::Numeric::Real, 'sign() on Complex with imaginary part throws';
throws-like { (3+4i).sign }, X::Numeric::Real, '.sign on Complex with imaginary part throws';

is sign(3+0i),   1, 'sign() of a positive real Complex is 1';
is sign(-2+0i), -1, 'sign() of a negative real Complex is -1';
is sign(0+0i),   0, 'sign() of zero Complex is 0';
is (5+0i).sign,  1, '.sign of a positive real Complex is 1';

# Make sure ordinary numeric sign is unaffected.
is (-7).sign, -1, '.sign on a plain Int still works';
