use Test;

# .sum / sum() coerce string elements through the full Raku numeric grammar
# (like .Numeric), so a string holding a radix literal or an allomorph sums
# correctly instead of erroring on a base-10-only parse.

plan 9;

is (1, "0xff").sum, 256, 'a hex-string element sums via its numeric value';
is ("0b1111", "0o17").sum, 30, 'binary + octal string elements sum';
is (1, "1/2").sum, 1.5, 'a rational-string element sums';
is sum(0b1111, 5), 20, 'sum() of a binary literal and an Int';
is ("10", "20").sum, 30, 'plain decimal strings still sum';
is (1, 2, 3).sum, 6, 'plain Int list still sums';
is (1, 3, pi).sum, 1 + 3 + pi, 'Num element still promotes';
is (1.5, 2.5).sum, 4.0, 'Rat elements still sum';

# a genuinely non-numeric string still throws X::Str::Numeric
dies-ok { (1, "x").sum }, 'a non-numeric string still throws';
