use v6;
use Test;

plan 8;

# The 2-argument `exp($power, $base)` is `$base ** $power`. With integer args it
# must stay EXACT (an arbitrary-precision Int), like raku's `**` — mutsu used to
# compute it as an f64 `powf` and return a lossy Num (`exp(72, 10)` was `1e72`,
# not `10 ** 72`). Regression driver: Math::Root's `exp(4 * $n, 10)`.

is exp(72, 10).^name, 'Int', 'exp(Int, Int) returns an Int';
is exp(72, 10), 10 ** 72, 'exp(72, 10) == 10 ** 72 exactly';
is exp(3, 2), 8, 'exp(3, 2) == 8';
is exp(0, 10), 1, 'exp(0, base) == 1';

# Non-integer arguments still yield a Num.
is exp(2.5, 10).^name, 'Num', 'exp(Rat, Int) is a Num';
ok (exp(2, 2.0) - 4).abs < 1e-9, 'exp(Int, Num) numeric value is correct';

# 1-argument exp is unchanged (e ** $x).
ok (exp(0) - 1).abs < 1e-12, 'exp(0) == 1';
ok (exp(1) - 2.718281828459045).abs < 1e-12, 'exp(1) == e';
