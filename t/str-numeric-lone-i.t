use Test;

plan 12;

# Numifying a string with a "lone i" — an imaginary unit written without an
# explicit coefficient — yields a coefficient of 1 (or -1 with a leading sign),
# both as a pure imaginary and as the imaginary part of a full complex number.
# Rakudo skips these ("cannot handle lone i yet"); mutsu handles them
# (roast S32-str/numeric.t "can handle − (U+2212) minus as regular minus").

is-deeply +'i',    0+1i, 'lone i';
is-deeply +'+i',   0+1i, 'lone +i';
is-deeply +'-i',   0-1i, 'lone -i';
is-deeply +'−i',   0-1i, 'lone -i with U+2212 minus';

is-deeply +'3+i',  3+1i, 'number + i';
is-deeply +'3-i',  3-1i, 'number - i';
is-deeply +'−10−i', -10-1i, 'both parts with U+2212 minus';

# Explicit coefficients still work unchanged.
is-deeply +'10i',   0+10i, 'number with i (coefficient present)';
is-deeply +'3+2i',  3+2i,  'both parts with coefficients';

# Non-numeric strings ending in "i" still fail (not treated as complex).
dies-ok { +'hi' },   'a non-numeric string ending in i still fails';
dies-ok { +'abci' }, 'another non-numeric i-suffixed string fails';
dies-ok { +'i-j' },  'a bogus complex-ish string fails';
