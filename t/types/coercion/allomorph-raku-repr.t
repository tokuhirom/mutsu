use Test;

# A RatStr's numeric half must use the exact Rat `.raku` representation.
# In particular, zero-denominator Rats are not Num infinities or NaN.

plan 6;

is RatStr.new(1/0, "1/0").raku,
    'RatStr.new(<1/0>, "1/0")',
    'RatStr preserves a positive zero-denominator Rat in .raku';
is RatStr.new(-1/0, "-1/0").raku,
    'RatStr.new(<-1/0>, "-1/0")',
    'RatStr preserves a negative zero-denominator Rat in .raku';
is RatStr.new(0/0, "0/0").raku,
    'RatStr.new(<0/0>, "0/0")',
    'RatStr preserves a zero-over-zero Rat in .raku';

is < 1/0 >.raku,
    'RatStr.new(<1/0>, "1/0")',
    'a padded fraction allomorph preserves its numeric Rat in .raku';
is (1/0).raku, '<1/0>', 'a direct Rat still uses its exact .raku representation';
is (1/0, -1/0, 0/0).raku,
    '(<1/0>, <-1/0>, <0/0>)',
    'nested zero-denominator Rats use their exact .raku representations';
