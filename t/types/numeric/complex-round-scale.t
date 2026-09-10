use v6;
use Test;

plan 6;

# Complex.round($scale) with a Rat scale must not accrete float noise:
# the imaginary part was -3.9000000000000004 instead of -3.9.
is (1.256-3.875i).round(0.1).gist, '1.3-3.9i', 'Complex.round(0.1) is exact';
is (1.2-3.8i).round.gist,          '1-4i',     'Complex.round (no scale)';
is (1.44+2.66i).round(0.1).gist,   '1.4+2.7i', 'Complex.round(0.1) positive parts';
is (12.34+56.78i).round(0.01).gist, '12.34+56.78i', 'Complex.round(0.01)';

# A Num (not Rat) scale keeps raku's float behavior — 0.1e0 is a Num, so
# `k * 0.1` noise is expected and matches raku (the exact path is Rat-only).
is (1.256-3.875i).round(0.1e0).gist, '1.3-3.9000000000000004i',
    'Complex.round with a Num scale matches raku (float)';

# Real (non-Complex) rounding with a Rat scale stays exact too
is 2.35e0.round(0.1), 2.4, 'Num.round(0.1) stays clean';
