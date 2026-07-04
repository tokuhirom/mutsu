use Test;

plan 13;

# A numeric fraction string is only `[sign]INT/INT`: a plain-integer numerator
# over an *unsigned* plain-integer denominator. A decimal part in either, or a
# signed denominator, is not a numeric literal and must fail numification
# (roast S32-str/numeric.t; Rakudo `#?rakudo todo`s these, "Unsure of what
# val() should accept").

# Accepted forms:
is-deeply +'3/2',   3/2,   '"3/2" numifies to 3/2';
is-deeply +'+3/2',  3/2,   '"+3/2" numifies to 3/2';
is-deeply +'-3/2', -3/2,   '"-3/2" numifies to -3/2';
is-deeply +'10/4',  10/4,  '"10/4" numifies to 5/2';
is +'3_0/2', 15,           'underscores in the numerator still parse (30/2 = 15)';

# Rejected forms (should throw X::Str::Numeric):
dies-ok { +'-3/-2' }, '"-3/-2" (signed denominator) fails';
dies-ok { +'3/-2'  }, '"3/-2" (signed denominator) fails';
dies-ok { +'+3/-2' }, '"+3/-2" (signed denominator) fails';
dies-ok { +'3.0/2' }, '"3.0/2" (decimal numerator) fails';
dies-ok { +'3/2.0' }, '"3/2.0" (decimal denominator) fails';
dies-ok { +'3/+2'  }, '"3/+2" (explicitly-signed denominator) fails';
dies-ok { +'3.5/2' }, '"3.5/2" (decimal numerator) fails';
dies-ok { +'1/2/3' }, '"1/2/3" fails';
