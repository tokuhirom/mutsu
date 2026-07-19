use v6;
use Test;

# `.round($scale)` / `round($x, $scale)` must stay exact for integer and
# rational operands, matching Rakudo's `Real.round(Real)` semantics
# `(self / $scale + 1/2).floor * $scale`, rather than routing through f64.

plan 18;

# Rat scale keeps the result an exact Rat (not 989.4300000000001).
is round(1000, 23.01), 989.43,           'round(Int, Rat) is exact';
is round(1000, 23.01).WHAT.^name, 'Rat', 'Rat scale yields a Rat';
is 1.07.round(0.1), 1.1,                  'Rat target, Rat scale is exact';

# Large Int must not lose precision through f64 (2^53 boundary).
is 9930972392403501.round(1), 9930972392403501, 'large Int round(1) stays exact';
is 9930972392403501.round(1).WHAT.^name, 'Int', 'Int scale yields an Int';

# Integer-scale rounding.
is 21.round(10), 20,      'round down to nearest 10';
is 255.round(10), 260,   'round up to nearest 10';
is (-21).round(10), -20, 'negative round to nearest 10';
is 21.round(10).WHAT.^name, 'Int', 'integer scale yields Int';

# Default scale (1).
is 1.7.round, 2,     'round half up (positive)';
is 2.5.round, 3,     'round .5 up';
is (-2.5).round, -2, 'round -.5 toward +Inf';

# Rat scale that lands on an integer value is still a Rat in Rakudo.
is 10.round(2.5), 10,             '10.round(2.5) == 10';
is 10.round(2.5).WHAT.^name, 'Rat', 'Rat scale keeps Rat even when integral';

# Num scale falls back to the float path (matches Rakudo's Num result).
is 9930972392403501.round(1e0).Int, 9930972392403500, 'Num scale uses float path';

# Allomorph operands unwrap correctly.
is <1000>.round(<10>), 1000, 'IntStr target and scale round exactly';

# Function form matches the method form.
is round(21, 10), 20,        'round() function form, integer scale';
is round(1.07, 0.1), 1.1,    'round() function form, Rat scale';
