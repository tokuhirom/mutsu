use Test;

# Rakudo's `Rational.Str` does NOT print the full exact expansion of a
# terminating decimal. It rounds every Rat to a fixed number of fractional
# digits and strips trailing zeros. The digit count is
#   Rat:    |denom| < 100_000 ?? 6 !! chars(|denom|) + 1
#   FatRat: |denom| < 100_000 ?? 6 !! chars(|denom|) + chars(whole) + 5
# (rakudo src/core.c/Rational.rakumod). Previously mutsu printed the full
# terminating expansion, e.g. 65501/256 => "255.86328125" instead of
# "255.863281". Found via Type/Str.rakudoc parse-base example.

plan 22;

# denom < 100_000 -> 6 fractional digits, rounded, trailing zeros stripped
is (65501/256).Str, '255.863281', 'terminating decimal rounds to 6 digits';
is (1/256).Str,     '0.003906',   '1/256 rounds to 6 digits';
is (1/128).Str,     '0.007813',   '1/128 rounds (exact would be 0.0078125)';
is (1/64).Str,      '0.015625',   '1/64 fits exactly in 6 digits';
is (3/8).Str,       '0.375',      'short terminating decimal is unpadded';
is (1/8).Str,       '0.125',      '1/8 exact';
is (1/2).Str,       '0.5',        '1/2 exact';
is (-1/128).Str,    '-0.007813',  'sign is preserved';

# non-terminating -> still 6 digits
is (1/3).Str,   '0.333333', '1/3 to 6 digits';
is (22/7).Str,  '3.142857', '22/7 to 6 digits';
is (1/7).Str,   '0.142857', '1/7 to 6 digits';

# 'FF.DD'.parse-base(16) is the doc example that surfaced the bug
is 'FF.DD'.parse-base(16).Str, '255.863281', 'parse-base terminating Rat';

# denom >= 100_000 -> chars(denom)+1 digits (so a value that would round to
# zero at 6 digits keeps enough digits to be represented)
is (1/10000000).Str,          '0.0000001',   'tiny value keeps precision (chars+1)';
is (1234567/1000000000).Str,  '0.001234567', 'chars(denom)+1 exact expansion';
is (1/160000).Str,            '0.0000063',   '1/160000 rounds at chars+1 digits';
is (123/2000000).Str,         '0.0000615',   '123/2000000 exact at chars+1';
is (3/10000000).Str,          '0.0000003',   'value that is 0 at 6 digits is extended';

# say / string interpolation use the same path
is ~(65501/256), '255.863281', '~ (prefix) matches .Str';
is "$(1/128)",   '0.007813',   'interpolation matches .Str';

# FatRat uses a wider digit budget: chars(denom)+chars(whole)+5
is FatRat.new(1, 128).Str,     '0.007813',   'small FatRat still 6 digits';
is FatRat.new(1, 1600000).Str, '0.000000625', 'FatRat wider budget keeps full expansion';
is FatRat.new(123456789, 1000000000000).Str, '0.000123456789', 'FatRat chars+chars+5 budget';
