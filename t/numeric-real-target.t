use Test;

plan 6;

# X::Numeric::Real carries the target type as a *type object* so that
# `target => Num` / `target => Real` matchers in throws-like succeed.
throws-like 'use fatal; (1+2i).Num',  X::Numeric::Real, target => Num;
throws-like 'use fatal; (1+2i).Real', X::Numeric::Real, target => Real;

# The exception is still thrown (without explicit matcher) and stringifies.
throws-like '(3+4i).Num', X::Numeric::Real;
throws-like '(3+4i).Real', X::Numeric::Real;

# A Complex with a zero imaginary part coerces cleanly.
is (5+0i).Num, 5e0, 'real Complex coerces to Num';
is (7+0i).Real, 7, 'real Complex coerces to Real';
