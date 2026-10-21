use Test;

# A role mixed into an argument is nearer than the value's own type, but the
# role and an unrelated nominal type tie: the declared types decide (#14176).
plan 5;

role R {}
multi f(R $a, Numeric $b) { "R" }
multi f(Rational:D $a, Rational:D $b) { "rat" }
is f(1.5 but R, 2.5), "rat", 'Rational:D pair beats (R, Numeric)';

multi g(R $a, Numeric $b) { "R" }
multi g(Real $a, Rational $b) { "real" }
is g(1.5 but R, 2.5), "real", 'Real/Rational beats (R, Numeric)';

multi h(R $a, Numeric $b) { "R" }
multi h(Numeric $a, Rational $b) { "num" }
is h(1.5 but R, 2.5), "num", 'Numeric/Rational beats (R, Numeric)';

is f(1.5, 2.5), "rat", 'a plain Rat still picks the Rational pair';
is f(1.5 but R, 2), "R", 'only (R, Numeric) binds an Int second argument';
