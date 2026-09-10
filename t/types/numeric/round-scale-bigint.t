use Test;

# .round($scale) on a very large integer must stay exact. The i128 fast path
# overflows for such values and previously fell through to an f64 round, where
# the BigInt numifies to Inf — so `(17**1500).round(10)` returned `Inf` (and,
# coerced to Int, saturated to i64::MAX). A BigInt path now keeps it exact.

plan 12;

# --- large BigInt targets, integer scale ---
is (17 ** 1500).round(10),
   (17 ** 1500).round(10).Str.Int,   # sanity: it is a real integer, not Inf
   'round(10) of a huge BigInt is an integer, not Inf';

# raku's round is floor(x/scale + 1/2) * scale.
my $x = 10 ** 40 + 7;               # ...0007
is $x.round(10),  10 ** 40 + 10,    'round(10) rounds ..07 up to the next ten';
is (10 ** 40 + 3).round(10), 10 ** 40, 'round(10) rounds ..03 down';

is (10 ** 30).round(10), 10 ** 30, 'round(10) of an exact power of ten is itself';
is (10 ** 30).round(1),  10 ** 30, 'round(1) of a big int is itself';

# --- negative big ints round half-up (toward +Inf on the .5 tie) ---
is (-(10 ** 40) - 7).round(10), -(10 ** 40) - 10, 'negative big round(10), 7 below rounds away';
is (-(10 ** 40) - 3).round(10), -(10 ** 40),      'negative big round(10), 3 below rounds toward zero';

# --- small ints still work (i128 fast path) ---
is (1234567).round(100),  1234600, 'small int round(100)';
is (-1234567).round(100), -1234600, 'small negative int round(100)';
is (7).round(10), 10, 'round(10) of 7';
is (4).round(10), 0,  'round(10) of 4';

# --- round() with no scale still works on a big int ---
is (17 ** 1500).round, 17 ** 1500, 'round() with no scale on a big int is identity';

done-testing;
