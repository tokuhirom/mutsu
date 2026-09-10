use Test;

plan 4;

# <...> words produce allomorphic types (IntStr, RatStr, etc.)
# Fraction notation <3/2> produces plain Rat (not allomorphic).
is <42>, 42, '<42> numifies to 42';
is-deeply <3/2>, 3/2, '<3/2> is a plain Rat';
is-deeply <1/0>, 1/0, '<1/0> is a plain Rat';
throws-like { 10 / 0e0 }, X::Numeric::DivideByZero, '/ with Num zero throws X::Numeric::DivideByZero';
