use Test;

# Complex exponentiation with an integer exponent must be computed by repeated
# multiplication so the result stays exact, like raku. The polar `exp(b*ln a)`
# form introduced floating-point noise: `(0+1i)**2` was
# `-1+0.00000000000000012246467991473532i` instead of `-1+0i`.

plan 12;

is (0+1i) ** 2, -1+0i,  'i**2 == -1';
# i**3 is -i; its gist keeps the signed-zero real part (`-0-1i`), which a
# `-0-1i` literal would not round-trip, so compare the rendered gist.
is ((0+1i) ** 3).gist, '-0-1i', 'i**3 == -i (exact, no float noise)';
is (0+1i) ** 4,  1+0i,  'i**4 == 1';
is (1+1i) ** 2,  0+2i,  '(1+i)**2 == 2i';
is (2+3i) ** 2, -5+12i, '(2+3i)**2';
is (1+2i) ** 3, -11-2i, '(1+2i)**3';
is (3+0i) ** 2,  9+0i,  'real-valued complex squares exactly';
is (1+1i) ** 10, 0+32i, '(1+i)**10';
is (0+1i) ** 0,  1+0i,  'anything**0 == 1';

# Negative integer exponent (reciprocal of the integer power).
is (0+1i) ** -1, 0-1i,  'i**-1 == -i';
is (1+1i) ** -2, 0-0.5i, '(1+i)**-2';

# A non-integer exponent still uses the polar form (unchanged).
is-approx ((2+3i) ** 0.5).abs, sqrt(sqrt(13)), 'fractional exponent via polar';
