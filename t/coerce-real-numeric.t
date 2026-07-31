use v6;
use Test;

plan 8;

# Real(...) / Numeric(...) coercion calls delegate to the .Real / .Numeric
# method form (Cro::Policy::Timeout uses Real($default) in BUILD).
is Real(5), 5, 'Real(Int) passes through';
is Real("2.5"), 2.5, 'Real(Str) parses the number';
is Real(Inf), Inf, 'Real(Inf) stays Inf';
is Real(3/2), 3/2, 'Real(Rat) stays Rat';
is Numeric("5"), 5, 'Numeric(Str) parses the number';
is Numeric(2.5), 2.5, 'Numeric(Rat literal) passes through';
is Real().gist, '(Real(Any))', 'Real() with no args is the coercion type term';
is Real(Int).gist, '(Real(Int))', 'Real(Int) is a parametric coercion type';
