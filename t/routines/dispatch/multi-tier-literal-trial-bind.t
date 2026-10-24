use Test;

# A tier of the multi candidates that holds a literal or `where` candidate is
# trial-bound in declaration order, so a tie inside it is not ambiguous
# (CSS::Module's `resolved-type`, #13280); a tier without one still is.
plan 7;

multi lit(Numeric:D $a, $, Numeric:D $b) { 'definite' }
multi lit(Numeric, $, Numeric)           { 'plain' }
multi lit($, $, 99)                      { 'literal' }
is lit(2, '+', 3), 'definite', 'an unrelated literal candidate puts the tie in a trial-bound tier';

multi wh(Int $a, $, $b where 99) { 'where' }
multi wh(Numeric:D $a, $, Numeric:D $b) { 'definite' }
multi wh(Numeric, $, Numeric)           { 'plain' }
is wh(2, '+', 3), 'definite', 'a `where` candidate in the same tier does the same';

multi amb(Numeric:D $a, $, Numeric:D $b) { 'definite' }
multi amb(Numeric, $, Numeric)           { 'plain' }
throws-like { amb(2, '+', 3) }, X::Multi::Ambiguous, 'without one, the tie is ambiguous';

multi nom(Str $s where 'x', $, $)       { 'str' }
multi nom(Numeric:D $a, $, Numeric:D $b) { 'definite' }
multi nom(Numeric, $, Numeric)           { 'plain' }
throws-like { nom(2, '+', 3) }, X::Multi::Ambiguous, 'a candidate that cannot take the arguments does not count';

multi ar(0, $, $, $)                    { 'four' }
multi ar(Numeric:D $a, $, Numeric:D $b) { 'definite' }
multi ar(Numeric, $, Numeric)           { 'plain' }
throws-like { ar(2, '+', 3) }, X::Multi::Ambiguous, 'nor does one of another arity';

multi narrow(Int $a, $, Int) { 'narrow' }
multi narrow(Numeric:D $a, $, Numeric:D $b) { 'definite' }
is narrow(2, '+', 3), 'narrow', 'a narrower candidate still wins outright';
is lit(2, '+', 99), 'definite', 'the first declared candidate of the tier that binds wins';
