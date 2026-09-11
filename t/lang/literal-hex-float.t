use Test;

# C99 hexadecimal floating point literals - rakudo/rakudo#6524.
# Mirrors the table in roast/S02-literals/numeric.t, plus the edge cases that
# only a single correctly-rounded conversion gets right.

plan 29;

isa-ok 0x1.8p+1, Num, 'hexfloat literal produces a Num';

is-deeply 0x1.8p+1,  3e0,    'hexfloat with fraction';
is-deeply 0x1p-2,    0.25e0, 'hexfloat without fraction, negative exponent';
is-deeply 0x.8p+1,   1e0,    'hexfloat without integer part';
is-deeply 0x1.8P4,   24e0,   'capital P exponent without sign';
is-deeply -0x1.8p+1, -3e0,   'negated hexfloat';
is-deeply 0X1.8p+1,  3e0,    'capital X prefix';

is-deeply 0x1.999999999999ap-4, 0.1e0, 'full-precision mantissa';
is-deeply 0xde_ad.be_efp+0, 0xdead.beefp0, 'underscores in mantissa';
is-deeply 0x1p+1_0, 1024e0, 'underscore in exponent';

is-deeply 0x1.fffffffffffffp+1023, 1.7976931348623157e308, 'largest double';
is-deeply 0x1p-1022, 2.2250738585072014e-308, 'smallest normal';
is-deeply 0x1p-1074, 5e-324, 'smallest subnormal';
is-deeply 0x1.8p-1074, 1e-323, 'subnormal tie rounds to even';
is-deeply 0x0.8p-1074, 0e0, 'half the smallest subnormal ties down to zero';
is-deeply 0x0.81p-1074, 5e-324, 'just over half rounds up';

is-deeply 0x1p+9999, Inf, 'exponent overflow gives Inf';
is-deeply 0x1p-9999, 0e0, 'exponent underflow gives 0';
is-deeply -0x1p+9999, -Inf, 'negated overflow gives -Inf';
ok (-0x0p+0) === -0e0, 'negative zero survives';

# Rounding of a mantissa wider than the 53 bits a double holds: the exact value
# is 2^53 + 1, which has no double, and ties-to-even takes it down to 2^53.
is-deeply 0x2000000000000_1p+0, 9007199254740992e0, 'over-wide mantissa rounds to even';
is-deeply 0x2000000000000_3p+0, 9007199254740996e0, 'over-wide mantissa rounds up';

# The literal is a term like any other, so it composes.
is-deeply 0x1p+1 + 0x1p+1, 4e0, 'hexfloats add';
is-deeply (0x1p+2, 0x1p+3), (4e0, 8e0), 'hexfloats in a list';

# Unchanged behaviours: the p/P exponent is the only thing that makes a hex
# literal a float.
is-deeply 0x1.abs, 1, 'method call on hex integer literal still works';
is-deeply 0xff, 255, 'plain hex integer literal still works';
is-deeply 0x1.Str, '1', 'another method call on a hex integer literal';

throws-like '0x1.8', Exception,
    'hexfloat without binary exponent is still an error';
throws-like '0x1p', Exception,
    'binary exponent without digits is still an error';

# vim: expandtab shiftwidth=4
