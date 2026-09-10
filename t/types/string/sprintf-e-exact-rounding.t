use v6;
use Test;

# `%e`/`%E` must round the exact numeric value (half away from zero) and take
# the exponent from the original value, matching Rakudo. In particular the
# mantissa is NOT re-normalized when a rounding carry pushes it to 10 or beyond
# (`sprintf("%.2e", 9.995)` is "10.00e+00", not "1.00e+01"). This differs from
# C printf and from Rust's default half-to-even f64 formatting.

plan 30;

# Exact-tie rounding on Rats (the digit after the cut is exactly 5).
is sprintf("%.3e", 1.2345),  "1.235e+00", "Rat tie rounds half up (.3e)";
is sprintf("%.2e", 2.675),   "2.68e+00",  "Rat tie rounds half up (.2e)";
is sprintf("%.3e", -1.2345), "-1.235e+00", "negative Rat tie rounds away from zero";
is sprintf("%+.3e", 1234.5), "+1.235e+03", "tie with + flag and exponent";

# Num arguments round their shortest-decimal representation, like %f.
is sprintf("%.2e", 1.005e0), "1.01e+00", "Num tie rounds half up";
is sprintf("%.0e", 2.5e0),   "3e+00",    "Num .0e rounds half up";

# Carry out of the mantissa is kept, NOT re-normalized.
is sprintf("%.2e", 9.995),   "10.00e+00", "mantissa carry keeps 10.00e+00";
is sprintf("%.2e", 9.9995),  "10.00e+00", "mantissa carry (deeper tie)";
is sprintf("%.3e", 9.9995),  "10.000e+00", "mantissa carry (.3e)";
is sprintf("%.0e", 9.995),   "10e+00",    "mantissa carry (.0e)";
is sprintf("%.2e", 999.5),   "10.00e+02", "carry keeps exponent, mantissa 10.00e+02";

# Basic values without rounding.
is sprintf("%e", 0),         "0.000000e+00", "zero";
is sprintf("%.2e", -0e0),    "-0.00e+00",    "negative zero keeps sign";
is sprintf("%.6e", 27.1),    "2.710000e+01", "27.1 default-ish precision";
is sprintf("%E", 27.1),      "2.710000E+01", "%E uppercase";
is sprintf("%.2e", -2.71),   "-2.71e+00",    "negative value";
is sprintf("%.0e", 27.1),    "3e+01",        "27.1 .0e";
is sprintf("%.0e", -2.71),   "-3e+00",       "-2.71 .0e";

# Small and large exponents.
is sprintf("%.3e", 1.235e-04), "1.235e-04",  "small negative exponent";
is sprintf("%.2e", 0.00012345), "1.23e-04",  "small value";
is sprintf("%.3e", 0.00012355), "1.236e-04", "small value rounds half up";
is sprintf("%.3e", 12345678900000), "1.235e+13", "large exponent";
is sprintf("%.2e", 6.02e23), "6.02e+23", "very large exponent";
is sprintf("%.2e", 0.000000009995), "10.00e-09", "carry with negative exponent";

# Width and zero padding still apply on top of the exact rendering.
is sprintf("%020.2e", 3.1415), "0000000000003.14e+00", "zero-padded width";
is sprintf("%15.2e", -9.995),  "     -10.00e+00",       "space-padded width with carry";

# Repeating fractions (no tie) round correctly.
is sprintf("%.10e", 1/3), "3.3333333333e-01", "1/3 rounds correctly";
is sprintf("%.10e", 1/7), "1.4285714286e-01", "1/7 rounds correctly";

# %g scientific path is unaffected by this change.
is sprintf("%.3g", 1234.5), "1.23e+03", "%g still significant-digit based";
is sprintf("%g", 100),      "100",      "%g integer";
