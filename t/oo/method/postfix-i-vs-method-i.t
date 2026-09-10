use Test;

# `postfix:<i>` (imaginary) is an OPERATOR, not a `.i` method. The bare dotted
# form `4.i` (and its hyper `».i`) must fail with X::Method::NotFound, while the
# postfix operator forms (`4i`, `4\i`, `(3)i`, `(2i)i`, `»i`/`>>i`) build Complex.

plan 12;

# Postfix operator forms build Complex.
is 4i,        Complex.new(0, 4), '4i literal';
is 4\i,       Complex.new(0, 4), '4\i unspace form';
is (3)i,      Complex.new(0, 3), '(expr)i on a paren term';
is (2i)i,     Complex.new(-2, 0), 'postfix i on an imaginary';
is (2i + 3)i, Complex.new(-2, 3), 'postfix i on a Complex';

# Hyper postfix operator (no dot) distributes.
is-deeply (2, 3)»i,  (Complex.new(0, 2), Complex.new(0, 3)), '(2,3)»i';
is-deeply (2, 3)>>i, (Complex.new(0, 2), Complex.new(0, 3)), '(2,3)>>i';

# The dotted method `.i` does not exist.
throws-like { 4.i }, X::Method::NotFound,
    message => *.starts-with("No such method 'i'"), '4.i throws';
throws-like { (2, 3)>>.i }, X::Method::NotFound, '>>.i throws';

# A user-defined method `i` is not shadowed by the operator.
my class D { method i { 42 } };
is D.i,   42, 'user method i works';
is D.i(), 42, 'user method i() works';

# A genuinely-lazy (infinite) sequence gists as a placeholder, not materialized.
is (1, 2, 3 ... *).gist, '(...)', 'infinite sequence gist is (...)';
